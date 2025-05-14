
# Data processing script for 2024 survey responses

# INIT ----
rm(list = ls())
if(!require("pacman")) install.packages("pacman")
pacman::p_load('tidyverse','here', 'dplyr', 'readr')

here::i_am("analysis/experimental/01_survey_data_processing.R")

DATA_PATH = here::here('data') # top-level directory for data
RESPONSE_DATA = file.path('2024_fall_clean', 'coursekata', 'raw',
                          'responses.csv') # path to response data file
CODEBOOK_DATA = file.path('2024_fall_clean', 'coursekata','codebooks', 'v5.6-all_survey.csv') # path to codebook data file
CONSTRUCT_DATA = file.path('questions_to_constructs.csv') # path to construct lookup file
OUTPUT_PATH = file.path(DATA_PATH,'2024_fall_data_processed') # directory for writing processed data
# Create the directory if it doesn't exist
if (!dir.exists(OUTPUT_PATH)) {
  dir.create(OUTPUT_PATH, recursive = TRUE)
}

# LOAD DATA ----

# Load raw data from college-24 repository
# NB: takes 20-30s
responses = read_csv(file.path(DATA_PATH, RESPONSE_DATA)) 

# Load codebook for interpreting survey responses
# NB: this is sourced from the fall 2024 coursekata experiment
responses_codebook = read_csv(file.path(DATA_PATH, CODEBOOK_DATA))

# Load CSV for mapping survey responses to internal psychological constructs
construct_lookup = read_csv(file.path(DATA_PATH, CONSTRUCT_DATA))

# PRE-SURVEY ----

## FILTER DATA  ----

# Select only response data from pre-course survey
pre_course_survey = responses |> filter(str_detect(lrn_activity_reference, 'Pre_Survey'))

# Select only codebook data for pre-course survey
pre_course_survey_codebook = responses_codebook |> filter(str_detect(lrn_activity_reference, 'Pre_Survey'))

## PROCESS DATA  ----

## > Process pre-course survey ----

# Split out individual survey item response and convert to integers, keep only relevant columns
# NB: this increases number of rows substantially
pre_course_survey_processed = pre_course_survey |>
  mutate(
    # Remove brackets and white space
    response_choice = gsub('\\[|\\]|\"', '', response),
    response_choice = trimws(response_choice)
  ) |>
  # Split by comma
  separate_rows(response_choice, sep = ',') |>
  mutate(response_choice = trimws(response_choice)) |>
  # Add order column
  group_by(class_id, release, student_id, lrn_question_reference) |>
  mutate(order = row_number()) |>
  ungroup() |>
  select(
    institution_id, class_id, release, book, student_id, 
    response, prompt,
    lrn_question_reference, lrn_question_position,
    response_choice, order
  )

## > Process codebook ----

# Add column for number of response options available on each survey question, keep only relevant columns
pre_course_survey_codebook_processed = pre_course_survey_codebook |>
  mutate(
    order = ifelse(is.na(order), 1, order) # added this in since some of the order values are missing KZ
  ) |>
  rowwise() |>
  mutate(
    # Count non-NA in columns indicating response options
    num_response_options = sum(!is.na(c_across(starts_with('lrn_option_')))),
  ) |> 
  select(release, variable_name, order,
         lrn_question_reference, lrn_type, prompt, 
         num_response_options,
         # starts_with('lrn_option_')
         )

## COMBINE DATA  ----
pre_course_survey_combined = pre_course_survey_processed |>
  # ignore release, left_join(pre_course_survey_codebook_processed, by = c('release', 'lrn_question_reference', 'order')) |>
  left_join(pre_course_survey_codebook_processed, by = c('lrn_question_reference', 'order')) |>
  dplyr::rename(
  prompt_survey_data = prompt.x,
  prompt_codebook = prompt.y,
  release = release.x,
  release_codebook = release.y  # This line is fine, comment not needed
)

# Summarize number of prior math courses per student-class
student_ma_prep_summary = pre_course_survey_combined |> 
  filter(str_detect(variable_name, 'ma_prep')) |> 
  group_by(student_id, class_id, lrn_question_reference) |> 
  reframe(
    rows = toString(n()),
    vals_list = strsplit(paste(variable_name, collapse = ','), split = ',')
  )

# Select first row of prior math responses from original dataframe and replace critical columns
# with summary values obtained above
pre_course_survey_ma_prep_combined = pre_course_survey_combined |>
  filter(str_detect(variable_name, 'ma_prep')) |> 
  group_by(student_id, class_id, lrn_question_reference) |>
  slice_head(n = 1) |> # Keep only the first row in each group
  ungroup() |>
  mutate(
    variable_name = 'ma_prep_combined',
    num_response_options = 4, # manually setting this
    
  ) |>
  left_join(student_ma_prep_summary, by=c('student_id', 'class_id', 'lrn_question_reference')) |>
  select(-response_choice, -response) |>
  dplyr::rename(
    response_choice = rows,
    response = vals_list
  )

# Remove ma_prep rows from original dataframe
pre_course_survey_combined = pre_course_survey_combined |> 
  filter(!str_detect(variable_name, 'ma_prep')) |> # should be 66751 - 2022 rows = 64729
  rbind(pre_course_survey_ma_prep_combined) # should be 64729 + 1439 (num rows for each individual student-class id in `pre_course_survey_ma_prep_combined`)

## ADD CONSTRUCT INFO  ----

pre_course_survey_combined = pre_course_survey_combined |> 
  left_join(construct_lookup, by = c('variable_name' = 'id'))

# Remove rows with survey items for which we don't assign a top-level construct
pre_course_survey_combined = pre_course_survey_combined |> 
  filter(!is.na(construct))

## SAVE DATA  ----

# Save as .RData
save(pre_course_survey_combined, file = file.path(OUTPUT_PATH, 'processed_college_24_pre_survey.RData'))
# Save as csv
write_csv(pre_course_survey_combined, file.path(OUTPUT_PATH, 'processed_college_24_pre_survey.csv'))
