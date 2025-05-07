#
# Analysis script for 2023 data
#

# INIT ----
rm(list = ls())
if(!require("pacman")) install.packages("pacman")
pacman::p_load('tidyverse','here', 'dplyr', 'readr',
               'lmerTest', 'emmeans', 'reshape', 'fields', 'MuMIn',
               'psych', 'cluster'
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
  )

# Race1 counts and proportions
final_demographics_data |>
  count(race1, name = "count") |>
  mutate(
    total_count = sum(count),
    percentage = count*100 / total_count
  )|>
  arrange(desc(percentage))

# year counts and proportions
final_demographics_data |>
  count(year, name = "count") |>
  mutate(
    total_count = sum(count),
    percentage = count*100 / total_count
  ) |>
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

# CORRELATIONS ----

construct_data <- model_data |>
  distinct(class_id, student_id, .keep_all = TRUE) |>
  select(starts_with("construct"))  # Select columns starting with 'construct'
  
rcorr_res <- corr.test(construct_data) #rcorr(as.matrix(construct_data), type = "pearson")
construct_cor_matrix <- rcorr_res$r
construct_p_matrix <- rcorr_res$p

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
set.seed(1234)

# means_centers = 7
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
pdf(file.path(DATA_OUT_PATH, 
              'observational_correlation_constructs_k7.pdf'),
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

# # Add correlation values to the heatmap
# for (i in 1:nrow(construct_ordered_matrix)) {
#   for (j in 1:ncol(construct_ordered_matrix)) {
#     text(
#       x = (j - 1) / (ncol(construct_ordered_matrix) - 1),  # Normalize x-coordinate
#       y = 1 - (i - 1) / (nrow(construct_ordered_matrix) - 1),  # Normalize y-coordinate (reverse)
#       labels = round(construct_ordered_matrix[i, j], 2),  # Round to 2 decimals
#       cex = 1.0,          # Font size
#       col = "#444444"       # Text color
#     )
#   }
# }

# Add correlation values with significance stars to the heatmap
for (i in 1:nrow(construct_ordered_matrix)) {
  for (j in 1:ncol(construct_ordered_matrix)) {
    
    # Extract p-value for current pair
    p_val <- construct_p_matrix[i, j]
    
    # Assign stars based on p-value
    if (is.na(p_val)) {
      stars <- ""
    } else if (p_val < 0.001) {
      stars <- "***"
    } else if (p_val < 0.01) {
      stars <- "**"
    } else if (p_val < 0.05) {
      stars <- "*"
    } else if (p_val < 0.1) {
      stars <- "."
    } else {
      stars <- ""
    }
    
    # Combine the correlation value and stars
    label <- paste0(round(construct_ordered_matrix[i, j], 2), stars)
    
    # Normalize coordinates for text placement
    text(
      x = (j - 1) / (ncol(construct_ordered_matrix) - 1),  # Normalize x-coordinate
      y = 1 - (i - 1) / (nrow(construct_ordered_matrix) - 1),  # Normalize y-coordinate (reverse)
      labels = label,  # Show both correlation value and stars
      cex = 1.0,       # Font size
      col = "#444444"  # Text color
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





# DYNAMICS ----
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

eng_plot

# MODELING ----

## AIC BOOTSTRAPPING----
### BOOTSTRAPPING ----

