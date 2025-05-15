
# analyze 2024 data

# INIT ----
rm(list = ls())
if(!require("pacman")) install.packages("pacman")
pacman::p_load('tidyverse','here', 'dplyr', 'readr', 'ggeffects', 'gridExtra')

here::i_am("analysis/experimental/05_analysis.R")

DATA_PATH = here::here('data') # top-level directory for data

CK_DATA = file.path('2024_fall_data_processed',
                    'processed_college_24_ck_combined.csv') # path to response data file
  
QUALTRICS_DATA = file.path('2024_fall_clean',
                            'intervention_survey',
                            'clean_processed_college_24_qualtrics.csv') # path to response data file

CANVAS_DATA_FILE = file.path('2024_fall_clean',
                             'canvas',
                            'clean_processed_2024_canvas_assessments.csv')

FIGURES_OUTPUT_PATH =here('results', '2024_fall','figures') 
if (!dir.exists(FIGURES_OUTPUT_PATH)) {
  dir.create(FIGURES_OUTPUT_PATH, recursive = TRUE)
}

# LOAD DATA ----
ck_data = read_csv(file.path(DATA_PATH, CK_DATA))

# qualtrics 
qualtrics_data = read_csv(file.path(DATA_PATH, QUALTRICS_DATA))

# canvas data 
canvas_data = read_csv(file.path(DATA_PATH,CANVAS_DATA_FILE))

# total number of students, must have done surveys
paste0('number of students: ', length(unique(c(qualtrics_data$student_id, ck_data$student_id)))) # 146

## SELECT DATA ----
ck_model_data = ck_data |>
  dplyr::select(
    # ids
    class_id, student_id, chapter_num, 
    # engagement
    proportion_pages_complete_student,
    construct_mean_anxiety_math,
    construct_mean_attitude_programming,
    construct_mean_experience_programming,
    construct_mean_interest_math,
    construct_mean_selfEfficacy,
    construct_mean_stress_expectation,
    construct_mean_value_task,
    eoc_score
  ) |>
  filter(!student_id == '2e6b31f0-4f70-4dd8-aa76-0631b24a1d90') # did not do an intervention activity 

ck_model_data_summary = ck_model_data |>
  group_by(class_id, student_id) |>
  mutate(
    avg_prop_pages_complete = mean(proportion_pages_complete_student, na.rm = TRUE),
    avg_eoc_score = mean(eoc_score, na.rm = TRUE)
  ) |> 
  dplyr::select(-proportion_pages_complete_student, -eoc_score) |> 
  distinct(class_id, student_id, .keep_all = TRUE) |> ungroup()

n_distinct(ck_model_data_summary$class_id, ck_model_data_summary$student_id) # 115

# CONSTRUCT CORRELATION ----
### PLOT ----
plt_ck_model_data_summary = ck_model_data_summary |>
  ungroup() |>
  select(student_id, avg_eoc_score, construct_mean_anxiety_math, construct_mean_stress_expectation) |>
  pivot_longer(
    cols = starts_with("construct"),  
    names_to = "construct_variable", 
    values_to = "value")  

pplot = ggplot(plt_ck_model_data_summary, aes(y = avg_eoc_score, x = value, color = construct_variable)) +
  geom_point(alpha = 0.2) +  
  scale_color_manual(
    values = c("construct_mean_anxiety_math" = "#FF953B", "construct_mean_stress_expectation" = "#D26A3B")
  ) + 
  scale_fill_manual(
    values = c(
      "construct_mean_anxiety_math" = "#FF953B",
      "construct_mean_stress_expectation" = "#D26A3B"
    )
  ) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2, aes(fill = construct_variable,group = construct_variable)) +  
  geom_smooth(method = "lm", se = FALSE, aes(group = construct_variable)) +  
  labs(title = "EOC Score vs constructs",
       y = "Average EOC Score",
       x = "construct responses") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) 

#pplot

ggsave(file.path(FIGURES_OUTPUT_PATH,"2024_experimental_correlation.pdf"), plot = pplot, width = 6, height = 4, dpi = 300)

### CORRELATIONS OF INITIAL PSYCHOLOGICAL ORIENTATION ----
ck_canvas_data_summary = ck_model_data_summary |>
  ungroup() |> 
  select(-construct_mean_attitude_programming, -construct_mean_experience_programming) |>
  drop_na() |>  
  left_join(canvas_data[c('student_id',"quiz_1_prop","quiz_2_prop","quiz_3_prop",
                          "quiz_5_prop","quiz_4_prop","quiz_mean","reali_prop")], by = c('student_id')
  ) |> 
  filter(!is.na(quiz_mean)) 

ck_canvas_data_summary_avg = ck_canvas_data_summary |> mutate(
  avg_eoc_quiz = rowMeans(pick(avg_eoc_score, quiz_mean), na.rm = TRUE)
)
 
n_distinct(ck_canvas_data_summary_avg$student_id) # 103 students 
 
cor.test(ck_canvas_data_summary_avg$avg_eoc_quiz, ck_canvas_data_summary_avg$construct_mean_stress_expectation) 
cor.test(ck_canvas_data_summary_avg$avg_eoc_quiz, ck_canvas_data_summary_avg$construct_mean_anxiety_math) 

### MODELING ---- 
lm_base = lm(data = ck_canvas_data_summary_avg, 
             avg_eoc_quiz 
             ~ 1 
)

lm_constructs = lm(data = ck_canvas_data_summary_avg, 
                   avg_eoc_quiz 
                   ~ 1 + 
                     construct_mean_anxiety_math +
                     construct_mean_stress_expectation
)

anova(lm_base, lm_constructs) # 3.22, p=.05

lm_all_constructs = lm(data = ck_canvas_data_summary_avg, 
                       avg_eoc_quiz 
                       ~ 1 + 
                         construct_mean_anxiety_math +
                         construct_mean_stress_expectation +
                         construct_mean_interest_math+
                         construct_mean_selfEfficacy +
                         construct_mean_value_task
)

anova(lm_constructs, lm_all_constructs) #0.96, p=0.41

# MANIPULATION CHECK ----
manipulation_data = qualtrics_data |>
  filter(source == 'qualtrics_midquarter') |>
  select(student_id, module_variation,
         construct_mean_stress_mindset,
         construct_mean_mindset_strategic,
         construct_mean_challengeSeeking,
         construct_mean_orientation_learningGoal,
         construct_mean_orientation_achievementGoal
  ) |>
  dplyr::rename(
    q_construct_mean_stress_mindset = construct_mean_stress_mindset,
    q_construct_mean_mindset_strategic = construct_mean_mindset_strategic,
    q_construct_mean_challengeSeeking = construct_mean_challengeSeeking,
    q_construct_mean_orientation_learningGoal = construct_mean_orientation_learningGoal,
    q_construct_mean_orientation_performanceGoal = construct_mean_orientation_achievementGoal
  ) |> 
  drop_na() |> 
  filter((module_variation == "A" | module_variation == "B")) 

n_distinct(manipulation_data |> filter(module_variation == "A") |>pull(student_id)) # 57 students in control
n_distinct(manipulation_data |> filter(module_variation == "B") |>pull(student_id)) # 63 students in intervention condition

# mean
manipulation_data_avg = manipulation_data |> 
  group_by(module_variation) |>
  summarize(
    sample_avg_stress_mindset = mean(q_construct_mean_stress_mindset, na.rm = TRUE),
    sample_avg_strategic_mindset = mean(q_construct_mean_mindset_strategic, na.rm = TRUE),
  )

## stress-as-enhancing mindset ----
stress_mindset_model_baseline = lm(q_construct_mean_stress_mindset ~ 1,
                           data = manipulation_data)

stress_mindset_model <- lm(q_construct_mean_stress_mindset ~ 1 + module_variation,
                           data = manipulation_data)

anova(stress_mindset_model_baseline, stress_mindset_model)

stress_predictions <- ggpredict(stress_mindset_model, terms = "module_variation")
stress_predictions = stress_predictions |> 
  left_join(manipulation_data_avg |> select(module_variation, sample_avg_stress_mindset), by = c('x' = 'module_variation'))
stress_predictions

## strategic mindset ----
strategic_mindset_baseline <- lm(q_construct_mean_mindset_strategic ~ 1,
                                 data = manipulation_data)

strategic_mindset_model <- lm(q_construct_mean_mindset_strategic ~  module_variation,
                              data = manipulation_data)

anova(strategic_mindset_baseline, strategic_mindset_model) 

strategic_predictions <- ggpredict(strategic_mindset_model, terms = "module_variation")
strategic_predictions = strategic_predictions |> 
  left_join(manipulation_data_avg |> select(module_variation, sample_avg_strategic_mindset), by = c('x' = 'module_variation'))
strategic_predictions

## OTHERS ----
### challenge seeking ----
challenge_seeking_baseline <- lm(q_construct_mean_challengeSeeking ~ 1,
                                 data = manipulation_data)

challenge_seeking_model <- lm(q_construct_mean_challengeSeeking ~ 1+ module_variation,
                              data = manipulation_data)
anova(challenge_seeking_baseline, challenge_seeking_model)

### learning goal ----
learningGoal_baseline <- lm(q_construct_mean_orientation_learningGoal ~ 1,
                            data = manipulation_data)

learningGoal_model <- lm(q_construct_mean_orientation_learningGoal ~ 1+ module_variation,
                         data = manipulation_data)
anova(learningGoal_baseline, learningGoal_model)

### performance goal ----

performanceGoal_baseline <- lm(q_construct_mean_orientation_performanceGoal ~ 1,
                               data = manipulation_data)

performanceGoal_model <- lm(q_construct_mean_orientation_performanceGoal ~ 1+ module_variation,
                            data = manipulation_data)

anova(performanceGoal_baseline, performanceGoal_model)

## PLOT BAR ----

# Create bar plot for Stress Mindset
stress_p <- ggplot(stress_predictions, aes(x = x, y = sample_avg_stress_mindset, fill = x)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0) +
  scale_y_continuous(limits = c(0, 0.9)) +  # Set y-axis range from 0 to 1
  scale_x_discrete(labels = c("A" = "Control", "B" = "Intervention")) +  # Relabel x-axis
  scale_fill_manual(
    values = c("A" = "#c8c8c8", "B" = "#d26a3b"),
    labels = c("A" = "Control", "B" = "Intervention")
  ) +
  labs(title = "Stress-as-enhancing", x = "Module Variation", y = "Rescaled ratings") +
  theme_minimal() +
  theme(legend.position = "none")

# Create bar plot for Strategic Mindset
strategic_p <- ggplot(strategic_predictions, aes(x = x, y = sample_avg_strategic_mindset, fill = x)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +  # Bars
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0) +  # Error bars
  scale_y_continuous(limits = c(0, 0.9)) +  # Set y-axis range from 0 to 1
  scale_x_discrete(labels = c("A" = "Control", "B" = "Intervention")) +  # Relabel x-axis
  scale_fill_manual(
    values = c("A" = "#c8c8c8", "B" = "#d26a3b"),
    labels = c("A" = "Control", "B" = "Intervention")
  ) +
  labs(title = "Strategic", x = "Module Variation", y = "") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend since fill is redundant

overall_bar = arrangeGrob(stress_p, strategic_p, ncol = 2)
#overall_bar

ggsave(file.path(FIGURES_OUTPUT_PATH,"2024_experimental_manipulation.pdf"), plot = overall_bar, width = 5, height = 6, dpi = 300)
