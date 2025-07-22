#
# Analysis script for 2023 data
#

# INIT ----
rm(list = ls())
if(!require("pacman")) install.packages("pacman")
pacman::p_load('tidyverse','here', 'dplyr', 'readr',
               'lmerTest', 'emmeans', 'reshape', 'fields', 'MuMIn',
               'psych', 'cluster', 'RColorBrewer'
               )
here::i_am("analysis/observational/05_analysis.R")

DATA_PATH = here('data', '2023-college-processed') # top-level directory for data
DATA_FILE = 'processed_college_23_combined.csv' # CSV file with combined data
DEMOGRAPHIC_FILE = 'processed_college_23_pre_survey_demographics.csv'

DATA_OUT_PATH = here('results', '2023-college')
# Create the directory if it doesn't exist
if (!dir.exists(DATA_OUT_PATH)) {
  dir.create(DATA_OUT_PATH, recursive = TRUE)
}
FIGURES_OUT_PATH = here('results', '2023-college', 'figures')
# Create the directory if it doesn't exist
if (!dir.exists(FIGURES_OUT_PATH)) {
  dir.create(FIGURES_OUT_PATH, recursive = TRUE)
}


# LOAD DATA ----

# survey
combined_data = read_csv(file.path(DATA_PATH, DATA_FILE))
glimpse(combined_data) # how many rows?
n_distinct(combined_data$class_id, combined_data$student_id) # 1306 unique students
n_distinct(combined_data$class_id) # 45 unique classes
n_distinct(combined_data$institution_id) # 11 unique institutions

# DEMOGRAPHICS FINAL
all_demographics_data = read_csv(file.path(DATA_PATH, DEMOGRAPHIC_FILE))
all_demographics_data_wide = all_demographics_data |>
  select(class_id, student_id, variable_name, response_label) |> 
  pivot_wider(
    names_from = variable_name,
    values_from = response_label
  )

final_demographics_data = combined_data |>
  select(class_id, student_id) |> 
  distinct() |> 
  left_join(all_demographics_data_wide, by = c('class_id','student_id'))

# calculate the proportion of students who report each value 
final_demographics_data |>
  count(gender, name = "count") |>
  mutate(
    total_count = sum(count),
    percentage = count*100 / total_count
  ) |>
  arrange(desc(percentage)) 

# Race1 counts and proportions
final_demographics_data |>
  count(race1, name = "count") |>
  mutate(
    total_count = sum(count),
    percentage = count*100 / total_count
  )|>
  arrange(desc(percentage)) 

# SELECT RELEVANT DATA ----  
# columns to be used in modeling and correlations 

model_data = combined_data |>
  select(
    # ids
    class_id, student_id, chapter_num,
    # engagement
    proportion_pages_complete_student,
    # attitudes
    construct_mean_anxiety_math,
    construct_mean_attitude_programming,
    construct_mean_experience_programming,
    construct_mean_interest_math,
    construct_mean_selfEfficacy,
    construct_mean_stress_expectation,
    construct_mean_value_task,
    eoc_score
  ) 
# sanity checks
glimpse(model_data)

# average engagement and eoc score 
model_data_summary = model_data |> 
  drop_na() |>
  group_by(class_id, student_id) |>
  mutate(
    avg_prop_pages_complete = mean(proportion_pages_complete_student, na.rm = TRUE),
    avg_eoc_score = mean(eoc_score, na.rm = TRUE)
  ) |> 
  select(-proportion_pages_complete_student, -eoc_score) |>
  distinct(class_id, student_id, .keep_all = TRUE) |>
  ungroup() |>
  drop_na() 

# CONSTRUCT CORRELATIONS ----

construct_data <- model_data |>
  distinct(class_id, student_id, .keep_all = TRUE) |>
  select(starts_with("construct"))  # Select columns starting with 'construct'
  
rcorr_res <- corr.test(construct_data) #rcorr(as.matrix(construct_data), type = "pearson")
construct_cor_matrix <- rcorr_res$r
construct_p_matrix <- rcorr_res$p

cat("\nCorrelation values:\n")
print(construct_cor_matrix)

cat("\nCorrelation p-values:\n")
print(construct_p_matrix)

## silhouette score ----

scaled_construct_cor_matrix = scale(construct_cor_matrix)
k.max = 6

# Finds the best k based on cluster separation.
sil_widths <- sapply(2:k.max, function(k){
  km_res <- kmeans(scaled_construct_cor_matrix, centers=k, nstart=50, iter.max=15)
  mean(silhouette(km_res$cluster, dist(scaled_construct_cor_matrix))[, 3])
})

optimal_k <- which.max(sil_widths) + 1  # Best silhouette score

print(paste0('optimal_k: ', optimal_k)) # 2

## run kmeans ----
means_centers = 2
kmeansObj <- kmeans(construct_cor_matrix, centers = means_centers)

# Create a data frame with variable names and their cluster assignments
construct_clusters_df <- data.frame(
  Variable = colnames(construct_cor_matrix),
  Cluster = kmeansObj$cluster
)
glimpse(construct_clusters_df)

# Get the order of variables based on clusters
construct_ordered_indices <- order(kmeansObj$cluster)
construct_ordered_names <- colnames(construct_cor_matrix)[construct_ordered_indices] # Get names in order

# # Open a PDF graphics device
pdf(file.path(FIGURES_OUT_PATH, 
              'observational_correlation_constructs_k2.pdf'),
    width = 7, height = 7)  # Adjust width, height, and resolution as needed

# Adjust margins and layout
par(mfrow = c(1, 1),           # Single plot layout
    mar = c(10, 10, 4,4) + 0.1, # Margins: bottom, left, top, right (extra space for labels and legend)
    oma = c(0, 0, 0, 0))       # No outer margins

construct_ordered_matrix <- construct_cor_matrix[construct_ordered_indices, construct_ordered_indices]

muted_blue = "#1E88E5" #"#578adb"
muted_red = "#D81B60" #"#d85c22"

# Plot the heatmap with a color legend
image(
  t(construct_ordered_matrix)[, nrow(construct_ordered_matrix):1],  # Reorder both rows and columns
  col = colorRampPalette(c(muted_blue, "white", muted_red))(100), 
  axes = FALSE,                # Suppress default axes
  xlab = "",                   # Remove default x-label
  ylab = "",                   # Remove default y-label
  main = "",                    # Title (adjust if needed)
  zlim = c(-1, 1)              # Set color scale range
)

# Add correlation values to the heatmap
for (i in 1:nrow(construct_ordered_matrix)) {
  for (j in 1:ncol(construct_ordered_matrix)) {
    text(
      x = (j - 1) / (ncol(construct_ordered_matrix) - 1),  # Normalize x-coordinate
      y = 1 - (i - 1) / (nrow(construct_ordered_matrix) - 1),  # Normalize y-coordinate (reverse)
      labels = round(construct_ordered_matrix[i, j], 2),  # Round to 2 decimals
      cex = 1.0,          # Font size
      col = "#444444"       # Text color
    )
  }
}

text(
  x = seq(0, 1, length.out = ncol(construct_ordered_matrix)), 
  y = -0.15,  # Place labels below the heatmap
  labels = construct_ordered_names, 
  srt = 45,   # Rotate labels by 45 degrees
  adj = 1, 
  xpd = TRUE, 
  cex =1.0   # Adjust font size
)

# Add y-axis labels
text(
  y = seq(0, 1, length.out = nrow(construct_ordered_matrix)), 
  x = -0.15,  # Place labels to the left of the heatmap
  labels = rev(construct_ordered_names), 
  srt = 0,    # No rotation
  adj = 1, 
  xpd = TRUE, 
  cex = 1.0   # Adjust font size
)

dev.off()


# DENSITY DYNAMICS ----
engagement_plot = ggplot(model_data, aes(x = proportion_pages_complete_student, color = factor(chapter_num))) +
  geom_density(
    size = 1, adjust = 1.5
    #size = 1
  ) + # KDE lines with different colors for conditions
  geom_hline(yintercept = -0.1, color = "black", size = 0.5) +
  scale_x_continuous(
    name = "Engagement (pages_complete)",
    limits = c(0, 1),                     # Set range from 0 to 1
    breaks = seq(0, 1, by = 0.25)         # Set ticks at 0.25 intervals
  ) +
  scale_color_manual(
    name = "chapter",
    values = rev(colorRampPalette(brewer.pal(9, "YlGn"))(20))[1:13]
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    text = element_text(size = 12),
    legend.position = "bottom", # Move legend to the bottom
    #panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.title.y = element_blank(), # Remove y-axis label
    axis.text.y = element_blank(),  # Remove y-axis ticks
    axis.ticks.y = element_blank()  # Remove y-axis tick marks
  )

engagement_plot

# Save file
ggsave(file.path(FIGURES_OUT_PATH, "engagement_density.pdf"), 
       plot = engagement_plot,
       width = 8,
       height = 6,
       dpi = 300,
       useDingbats = F
)

eoc_plot = ggplot(model_data, aes(x = eoc_score, color = factor(chapter_num))) +
  geom_density(#linewidth = 1,
    size = 1, adjust = 1.3
    #size = 1
  ) + # KDE lines with different colors for conditions
  geom_hline(yintercept = -0.1, color = "black", size = 0.5) +
  scale_x_continuous(
    name = "end-of-chapter quiz performance",
    labels = seq(0, 1, by = 0.25)
  ) +
  scale_color_manual(
    name = "chapter",
    values = rev(colorRampPalette(brewer.pal(9, "Blues"))(20))[1:13]
  ) +
  theme_minimal() +
  theme(
    plot.title = element_blank(),
    text = element_text(size = 15),
    legend.position = "bottom", # Move legend to the bottom
    #panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.title.y = element_blank(), # Remove y-axis label
    axis.text.y = element_blank(),  # Remove y-axis ticks
    axis.ticks.y = element_blank()  # Remove y-axis tick marks
  )

eoc_plot

# Save file
ggsave(file.path(FIGURES_OUT_PATH, "eoc_density.pdf"), 
       plot = eoc_plot,
       width = 8,
       height = 6,
       dpi = 300,
       useDingbats = F
)

# MODELING ----

## 1) engagement <- survey constructs ----
engagement_avg_baseline = lmer(
  data = model_data_summary,
  avg_prop_pages_complete ~ 1 +
    (1 | class_id),
  REML = F
)

engagement_avg_attitudes = lmer(
  data = model_data_summary,
  avg_prop_pages_complete ~ 1 +
    construct_mean_stress_expectation +
    construct_mean_selfEfficacy +
    # construct_mean_selfEfficacy_programming +
    construct_mean_anxiety_math +
    construct_mean_attitude_programming +
    construct_mean_interest_math +
    # construct_mean_interest_programming +
    construct_mean_value_task +
    construct_mean_experience_programming +
    # construct_mean_experience_math +
    (1 | class_id),
  REML = F
)

anova(engagement_avg_baseline, engagement_avg_attitudes)

# stress_expectation 
engagement_avg_stress_expectation = lmer(
  data = model_data_summary,
  avg_prop_pages_complete ~ 1 +
    construct_mean_stress_expectation +
    (1 | class_id),
  REML = F
)

# selfEfficacy
engagement_avg_selfEfficacy = lmer(
  data = model_data_summary,
  avg_prop_pages_complete ~ 1 +
    construct_mean_selfEfficacy +
    (1 | class_id),
  REML = F
)

# anxiety_math
engagement_avg_anxiety_math = lmer(
  data = model_data_summary,
  avg_prop_pages_complete ~ 1 +
    construct_mean_anxiety_math +
    (1 | class_id),
  REML = F
)

# attitude_programming
engagement_avg_attitude_programming = lmer(
  data = model_data_summary,
  avg_prop_pages_complete ~ 1 +
    construct_mean_attitude_programming +
    (1 | class_id),
  REML = F
)

# interest_math
engagement_avg_interest_math = lmer(
  data = model_data_summary,
  avg_prop_pages_complete ~ 1 +
    construct_mean_interest_math +
    (1 | class_id),
  REML = F
)

# value_task
engagement_avg_value_task = lmer(
  data = model_data_summary,
  avg_prop_pages_complete ~ 1 +
    construct_mean_value_task +
    (1 | class_id),
  REML = F
)

# experience_programming
engagement_avg_experience_programming = lmer(
  data = model_data_summary,
  avg_prop_pages_complete ~ 1 +
    construct_mean_experience_programming +
    (1 | class_id),
  REML = F
)

anova(engagement_avg_baseline, engagement_avg_stress_expectation)
anova(engagement_avg_baseline, engagement_avg_selfEfficacy)
anova(engagement_avg_baseline, engagement_avg_anxiety_math)
anova(engagement_avg_baseline, engagement_avg_attitude_programming)
anova(engagement_avg_baseline, engagement_avg_interest_math)
anova(engagement_avg_baseline, engagement_avg_value_task)
anova(engagement_avg_baseline, engagement_avg_experience_programming)

## 2) EOC <- survey constructs ----
eoc_avg_baseline = lmer(
  data = model_data_summary,
  avg_eoc_score ~ 1 +
    (1 | class_id),
  REML = F
)

eoc_avg_attitudes = lmer(data = model_data_summary, avg_eoc_score ~ 1 +
                           construct_mean_stress_expectation +
                           construct_mean_selfEfficacy +
                           # construct_mean_selfEfficacy_programming +
                           construct_mean_anxiety_math +
                           construct_mean_attitude_programming +
                           construct_mean_interest_math +
                           # construct_mean_interest_programming +
                           construct_mean_value_task +
                           construct_mean_experience_programming +
                           # construct_mean_experience_math +
                           (1 | class_id),
                         REML = F)

# do pre-course survey responses predict average quiz performance?
anova(eoc_avg_baseline, eoc_avg_attitudes)

eoc_avg_nervous_comb = lmer(data = model_data_summary,
                            avg_eoc_score ~ 1 +
                              construct_mean_anxiety_math +
                              construct_mean_stress_expectation +
                              (1 | class_id),
                            REML = F
)

# experience, value, self efficacy, programming
eoc_avg_exp_confval_comb = lmer(data = model_data_summary,
                                avg_eoc_score ~ 1 +
                                  construct_mean_experience_programming +
                                  construct_mean_selfEfficacy +
                                  construct_mean_value_task +
                                  construct_mean_interest_math +
                                  construct_mean_attitude_programming +
                                  (1 | class_id),
                                REML = F
)

# with engagement too 
eoc_avg_attitudes_eng = lmer(data = model_data_summary, avg_eoc_score ~ 1 +
                          avg_prop_pages_complete +
                           construct_mean_stress_expectation +
                           construct_mean_selfEfficacy +
                           # construct_mean_selfEfficacy_programming +
                           construct_mean_anxiety_math +
                           construct_mean_attitude_programming +
                           construct_mean_interest_math +
                           # construct_mean_interest_programming +
                           construct_mean_value_task +
                           construct_mean_experience_programming +
                           # construct_mean_experience_math +
                           (1 | class_id),
                         REML = F)

# calculate relative aic
eoc_avg_baseline_aic = AIC(eoc_avg_baseline) 
eoc_avg_attitudes_aic = AIC(eoc_avg_attitudes)
eoc_avg_nervous_comb_aic = AIC(eoc_avg_nervous_comb)
eoc_avg_exp_confval_comb_aic = AIC(eoc_avg_exp_confval_comb)

actual_AIC = data.frame(
  rel_eoc_avg_baseline_aic = eoc_avg_baseline_aic-eoc_avg_baseline_aic, # 0
  rel_eoc_avg_attitudes_aic = eoc_avg_attitudes_aic-eoc_avg_baseline_aic,  #
  rel_eoc_avg_nervous_comb_aic = eoc_avg_nervous_comb_aic-eoc_avg_baseline_aic, # 
  rel_eoc_avg_exp_confval_comb_aic = eoc_avg_exp_confval_comb_aic-eoc_avg_baseline_aic # 
) |>
  pivot_longer(
    cols = everything(),                 # pivot all columns
    names_to = "model",                 # new column for AIC type
    values_to = "aic_value"             # new column for values
  )

glimpse(actual_AIC)

## 3) EOC <- engagement ----
eoc_avg_engagement = lmer(
  data = model_data_summary,
  avg_eoc_score ~ 1 +
    avg_prop_pages_complete + 
    (1 | class_id),
  REML = F
)
  
anova(eoc_avg_baseline, eoc_avg_engagement)

## 4) EOC <- survey constructs + engagement ----
eoc_avg_engagement_attitudes = lmer(
  data = model_data_summary,
  avg_eoc_score ~ 1 +
    avg_prop_pages_complete + 
    construct_mean_stress_expectation +
    construct_mean_selfEfficacy +
    # construct_mean_selfEfficacy_programming +
    construct_mean_anxiety_math +
    construct_mean_attitude_programming +
    construct_mean_interest_math +
    # construct_mean_interest_programming +
    construct_mean_value_task +
    construct_mean_experience_programming +
    # construct_mean_experience_math +
    (1 | class_id),
  REML = F
)
anova(eoc_avg_attitudes, eoc_avg_engagement_attitudes) 


### AIC BOOTSTRAPPING ----
print('running AIC bootstrapping, will take a while ...')
bootstrap_iters = 100 # can be set to 1000 but will take a long time
set.seed(123)

model_data_summary_unique_classes = unique(model_data_summary$class_id)

model_data_summary_stud_bootstrap = replicate(
  n = bootstrap_iters,
  expr = {
    tryCatch({
      
      # Initialize a new dataframe for this iteration
      sampled_model_summary_data <- data.frame()
      
      for (class in model_data_summary_unique_classes){
        # get all unique students, sample and add back to a class df 
        class_df = model_data_summary |>
          filter(class_id == class)
        
        unique_students = unique(class_df$student_id)
        
        student_sample = sample_n(
          as_tibble(unique_students),
          size = length(unique_students),
          replace = TRUE
        ) 
        
        for (i in seq_along(student_sample$value)) {
          student = student_sample$value[i]
          # Filter for student and add a new unique ID column
          temp_data <- class_df |> 
            filter(student_id == student) |> 
            mutate(student_UID = paste0(class, student, "_", i))
          
          sampled_model_summary_data <- bind_rows(sampled_model_summary_data, temp_data)
        }
      }
      
      #view(sampled_model_summary_data)
      
      #view(sampled_model_summary_data)
      eoc_avg_baseline = lmer(
        data = sampled_model_summary_data,
        avg_eoc_score ~ 1 +
          (1 | class_id),
        REML = F
      )
      
      #print(summary(eoc_baseline))
      #print(paste('eoc_baseline: ', AIC(eoc_baseline)))
      
      eoc_avg_attitudes = lmer(data = sampled_model_summary_data, avg_eoc_score ~ 1 +
                                 construct_mean_stress_expectation +
                                 construct_mean_selfEfficacy +
                                 # construct_mean_selfEfficacy_programming +
                                 construct_mean_anxiety_math +
                                 construct_mean_attitude_programming +
                                 construct_mean_interest_math +
                                 # construct_mean_interest_programming +
                                 construct_mean_value_task +
                                 construct_mean_experience_programming +
                                 # construct_mean_experience_math +
                                 (1 | class_id),
                               REML = F)
      
      
      eoc_avg_nervous_comb = lmer(data = sampled_model_summary_data,
                                  avg_eoc_score ~ 1 +
                                    construct_mean_anxiety_math +
                                    construct_mean_stress_expectation +
                                    (1 | class_id),
                                  REML = F
      )
      
      eoc_avg_exp_comb = lmer(data = sampled_model_summary_data,
                              avg_eoc_score ~ 1 +
                                construct_mean_experience_programming +
                                (1 | class_id),
                              REML = F
      )
      
      eoc_avg_confval_comb = lmer(data = sampled_model_summary_data,
                                  avg_eoc_score ~ 1 +
                                    construct_mean_selfEfficacy +
                                    construct_mean_value_task +
                                    construct_mean_interest_math +
                                    construct_mean_attitude_programming +
                                    (1 | class_id),
                                  REML = F
      )
      
      # nervous x confval
      eoc_avg_nervous_confval_comb = lmer(data = sampled_model_summary_data,
                                          avg_eoc_score ~ 1 +
                                            construct_mean_anxiety_math +
                                            construct_mean_stress_expectation +
                                            construct_mean_selfEfficacy +
                                            construct_mean_value_task +
                                            construct_mean_interest_math +
                                            construct_mean_attitude_programming +
                                            (1 | class_id),
                                          REML = F
      )
      
      # nervous x exp
      eoc_avg_nervous_exp_comb = lmer(data = sampled_model_summary_data,
                                      avg_eoc_score ~ 1 +
                                        construct_mean_anxiety_math +
                                        construct_mean_stress_expectation +
                                        construct_mean_experience_programming +
                                        (1 | class_id),
                                      REML = F
      )
      
      # exp x confval
      eoc_avg_exp_confval_comb = lmer(data = sampled_model_summary_data,
                                      avg_eoc_score ~ 1 +
                                        construct_mean_experience_programming +
                                        construct_mean_selfEfficacy +
                                        construct_mean_value_task +
                                        construct_mean_interest_math +
                                        construct_mean_attitude_programming +
                                        (1 | class_id),
                                      REML = F
      )
      
      
      # calculate aic/etc, one per iter per model
      data.frame(
        eoc_avg_baseline_aic = AIC(eoc_avg_baseline), 
        #eoc_avg_baseline_bic = BIC(eoc_avg_baseline),
        eoc_avg_attitudes_aic = AIC(eoc_avg_attitudes),
        # #eoc_avg_attitudes_bic = BIC(eoc_avg_attitudes),
        eoc_avg_nervous_comb_aic = AIC(eoc_avg_nervous_comb),
        #eoc_avg_nervous_comb_bic = BIC(eoc_avg_nervous_comb),
        eoc_avg_exp_comb_aic = AIC(eoc_avg_exp_comb),
        #eoc_avg_exp_comb_bic = BIC(eoc_avg_exp_comb)
        eoc_avg_confval_comb_aic = AIC(eoc_avg_confval_comb),
        #eoc_avg_confval_comb_bic = BIC(eoc_avg_confval_comb),
        eoc_avg_nervous_confval_comb_aic = AIC(eoc_avg_nervous_confval_comb),
        #eoc_avg_nervous_confval_comb_bic = BIC(eoc_avg_nervous_confval_comb),
        eoc_avg_nervous_exp_comb_aic = AIC(eoc_avg_nervous_exp_comb),
        #eoc_avg_nervous_exp_comb_bic = BIC(eoc_avg_nervous_exp_comb),
        eoc_avg_exp_confval_comb_aic = AIC(eoc_avg_exp_confval_comb)
        #eoc_avg_exp_confval_comb_bic = BIC(eoc_avg_exp_confval_comb)
      )
    }, error = function(e) {
      # Step 6: Handle errors
      message("Error in iteration: ", e$message)
      
      # Print problematic data (optional)
      #   if (exists("sampled_model_summary_data")) {
      #     message("Sampled data causing the error:")
      #     print(head(sampled_model_summary_data))
      #   }
    }
    )
  },
  simplify = FALSE
)

model_data_summary_stud_bootstrap_df <- bind_rows(model_data_summary_stud_bootstrap)
#glimpse(model_data_summary_stud_bootstrap_df) # 100 rows

# # Save as .RData
# save(model_data_summary_stud_bootstrap_df, file = file.path(DATA_OUT_PATH, 'average_eoc_bootstrapped_students_aic_raw.RData'))
# # Save as csv
# write_csv(model_data_summary_stud_bootstrap_df, file = file.path(DATA_OUT_PATH, 'average_eoc_bootstrapped_students_aic_raw.csv'))

### Relative AIC ----
relative_eoc_avg_student_aic_data = model_data_summary_stud_bootstrap_df |>
  mutate(
    rel_eoc_avg_baseline_aic = eoc_avg_baseline_aic - eoc_avg_baseline_aic,
    rel_eoc_avg_attitudes_aic = eoc_avg_attitudes_aic - eoc_avg_baseline_aic,
    rel_eoc_avg_nervous_comb_aic = eoc_avg_nervous_comb_aic - eoc_avg_baseline_aic,
    rel_eoc_avg_exp_comb_aic = eoc_avg_exp_comb_aic - eoc_avg_baseline_aic,
    rel_eoc_avg_confval_comb_aic = eoc_avg_confval_comb_aic - eoc_avg_baseline_aic,
    rel_eoc_avg_nervous_confval_comb_aic = eoc_avg_nervous_confval_comb_aic - eoc_avg_baseline_aic,
    rel_eoc_avg_nervous_exp_comb_aic = eoc_avg_nervous_exp_comb_aic - eoc_avg_baseline_aic,
    rel_eoc_avg_exp_confval_comb_aic = eoc_avg_exp_confval_comb_aic - eoc_avg_baseline_aic,
    
    nervous_rel_eoc_avg_attitudes_aic = eoc_avg_attitudes_aic - eoc_avg_nervous_confval_comb_aic,
    nervous_rel_eoc_avg_nervous_confval_comb_aic = eoc_avg_nervous_confval_comb_aic -eoc_avg_nervous_confval_comb_aic
  ) |>
  select(starts_with('rel'), starts_with('nervous'))

# calculate average and quantiles 
relative_eoc_avg_student_aic_data_summary <- relative_eoc_avg_student_aic_data |>
  select(starts_with('rel'))|>
  summarise(
    across(
      where(is.numeric),
      ~ c(
        mean = mean(.x, na.rm = TRUE),
        lb = quantile(.x, probs = 0.025, na.rm = TRUE),
        ub = quantile(.x, probs = 0.975, na.rm = TRUE)
      ),
      .names = "{.col}" 
    )
  ) |>
  
  mutate(type = c("mean", 'lb', 'ub')) 

transposed_relative_eoc_avg_student_aic_data_summary <- relative_eoc_avg_student_aic_data_summary |>
  pivot_longer(
    cols = -type,            
    names_to = "model",      
    values_to = "value"     
  ) |>
  pivot_wider(
    names_from = type,       
    values_from = value     
  ) |>
  left_join(actual_AIC, by = 'model')

### GRAPH RELATIVE AIC ----
student_eoc_avg_survey_aic_plot <- ggplot(data = transposed_relative_eoc_avg_student_aic_data_summary |> filter(model %in% c("rel_eoc_avg_baseline_aic", 
                                                                                                                             "rel_eoc_avg_attitudes_aic",
                                                                                                                             "rel_eoc_avg_nervous_comb_aic",
                                                                                                                             "rel_eoc_avg_exp_confval_comb_aic") ), 
                                          aes( x = fct_inorder(model), y = aic_value)) + # actual instead of mean
  geom_point(alpha = 0) +  
  geom_segment(
    aes(
      x = as.numeric(fct_inorder(model)) - 0.35,  # Start of the line
      xend = as.numeric(fct_inorder(model)) + 0.35,  # End of the line
      y = aic_value,  # Constant y = mean
      yend = aic_value  # Constant y = mean
    ),
    size = 2,  # Line thickness,
    color = "#333333"  # Line color
  ) +
  geom_rect(aes(
    xmin = as.numeric(fct_inorder(model)) - 0.3,  # Adjust left boundary
    xmax = as.numeric(fct_inorder(model)) + 0.3,  # Adjust right boundary
    ymin = lb,  # Lower bound
    ymax = ub   # Upper bound
  ), alpha = 0.2, fill = "grey") +  
  geom_hline(
    # horizontal dotted line
    yintercept = 0,  # Horizontal line at y = 0
    linetype = "dashed",  # Dotted line
    color = "darkgrey",   # Line color
    size = 0.8            # Line thickness
  ) +
  # Set y-axis range
  scale_y_continuous(
    limits = c(-65, 5)  # Y-axis range from -65 to 0
  ) +
  theme_minimal() +  
  labs(y = "Relative AIC", x = "", title = "avg. eoc ~ survey (sampled students)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

print(student_eoc_avg_survey_aic_plot)

ggsave(
  filename = file.path(FIGURES_OUT_PATH, "2023_student_eoc_avg_survey_aic2.pdf"),
  plot = student_eoc_avg_survey_aic_plot,
  device = "pdf",
  width = 5,
  height = 5
)

