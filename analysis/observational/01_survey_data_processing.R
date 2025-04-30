#
# Data processing script for 2023 precourse survey responses
#

# INIT ----
rm(list = ls())
if(!require("pacman")) install.packages("pacman")
pacman::p_load('tidyverse','here', 'dplyr', 'readr')

here::i_am("analysis/observational/01_survey_data_processing.R")

DATA_PATH = here::here('data') # top-level directory for data
RESPONSE_DATA = file.path('2023-college', 'raw', 'responses.Rdata') # path to response data file
CODEBOOK_DATA = file.path('2023-college', 'codebooks', 'codebook_all_surveys.csv') # path to codebook data file
CONSTRUCT_DATA = file.path('questions_to_constructs.csv') # path to construct lookup file
OUTPUT_PATH =  here('data', '2023-college-processed')  # directory for writing processed data
# Create the directory if it doesn't exist
if (!dir.exists(OUTPUT_PATH)) {
  dir.create(OUTPUT_PATH, recursive = TRUE)
}


# LOAD DATA ----

# Load raw data from 2023-college repository
# NB: takes 20-30s
load(file.path(DATA_PATH, RESPONSE_DATA)) # loads `responses` dataframe
# # sanity checks
# glimpse(responses)


# Load codebook for interpreting survey responses --> TODO check if codebook_all_surveys.csv is different from codebook_all_surveys_updated.csv
# NB: this is sourced from outside the coursekata 2023-college repository
# and is instead part of the coursekata internal-research repository (internal-research/codebooks/survey-codebooks)
responses_codebook = read_csv(file.path(DATA_PATH, CODEBOOK_DATA))
# # sanity checks
# glimpse(responses_codebook)


# Load CSV for mapping survey responses to internal psychological constructs
construct_lookup = read_csv(file.path(DATA_PATH, CONSTRUCT_DATA))
# # sanity checks
# glimpse(construct_lookup)


# FILTER DATA ----

# Select only response data from pre-course survey
pre_course_survey = responses |> filter(str_detect(lrn_activity_reference, 'Pre_Survey'))
# 
# # sanity checks
# glimpse(pre_course_survey)
# table(pre_course_survey$page)
# n_distinct(pre_course_survey$class_id, pre_course_survey$student_id)
# 


# Select only codebook data for pre-course survey
pre_course_survey_codebook = responses_codebook |> filter(str_detect(lrn_activity_reference, 'Pre_Survey'))
# # sanity checks
# glimpse(pre_course_survey_codebook)



# PROCESS DATA ----

# > Process pre-course survey ----

# Split out individual survey item respones and convert to integers, keep only relevant columns
# NB: this increases number of rows substantially
pre_course_survey_processed = pre_course_survey |>
  filter(lrn_type != 'shorttext', lrn_type != 'plaintext') |> # remove survey question formats we won't use
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
    lrn_type, lrn_question_reference, lrn_question_position,
    response_choice, order
  )
# # sanity checks
# glimpse(pre_course_survey_processed)

# Replace 'null' and '' responses with NA
pre_course_survey_processed = pre_course_survey_processed |> 
  rowwise() |> 
  mutate(
    response_choice = ifelse(response_choice == 'null' | response_choice == '', NA, response_choice)
  )
# # sanity checks
# glimpse(pre_course_survey_processed)

# Filter out NA and convert response choice to integer
pre_course_survey_processed = pre_course_survey_processed |> 
  filter(!is.na(response_choice)) |> 
  rowwise() |> 
  mutate(
    response_choice = as.numeric(response_choice)
  )

# # sanity checks
# glimpse(pre_course_survey_processed)
# n_distinct(pre_course_survey_processed$class_id, pre_course_survey_processed$student_id)



# > Process codebook ----

# Add column for number of response options available on each survey question, keep only relevant columns
pre_course_survey_codebook_processed = pre_course_survey_codebook |>
  rowwise() |>
  mutate(
    # Count non-NA in separate `lrn_option_*` columns to determine number of response options
    num_response_options = sum(!is.na(c_across(starts_with('lrn_option_'))))
  ) |> 
  select(release, variable_name, order,
         lrn_type, lrn_question_reference, prompt, 
         num_response_options,
         # starts_with('lrn_option_')
  )
# # sanity checks
# glimpse(pre_course_survey_codebook_processed)



# COMBINE DATA ----

# SEE DEMOGRAPHIC DATA
# pre_course_survey_combined = pre_course_survey_processed |>
#   left_join(pre_course_survey_codebook_processed, by = c('release', 'lrn_question_reference', 'order'))
# write_csv(pre_course_survey_combined, file.path(OUTPUT_PATH, 'processed_college_23_pre_survey_ALL_QUESTIONS.csv'))

# Combine survey data with codebook variables
pre_course_survey_combined = pre_course_survey_processed |>
  left_join(pre_course_survey_codebook_processed, by = c('release', 'lrn_question_reference', 'order')) |> 
  rename(
    prompt_survey_data = prompt.x,
    prompt_codebook = prompt.y,
    lrn_type_survey_data = lrn_type.x,
    lrn_type_codebook = lrn_type.y
  ) |> 
  ungroup()

# # sanity checks
# glimpse(pre_course_survey_combined)
# n_distinct(pre_course_survey_combined$class_id, pre_course_survey_combined$student_id)
 
# Handle math prep questions
# There are between 1 and 4 separate rows for each student-class indicating their responses
# to the prior math coursework questions.
# Here, we collapse those to 1 row summarizing their number of previous courses
# And replace the first instance of this survey question with the collapsed data

# Summarize number of prior math courses per student-class
student_ma_prep_summary = pre_course_survey_combined |> 
  filter(str_detect(variable_name, 'ma_prep')) |> 
  group_by(student_id, class_id, lrn_question_reference) |> 
  reframe(
    rows = toString(n()),
    vals_list = strsplit(paste(variable_name, collapse = ','), split = ',')
  )
# # # sanity checks
# glimpse(student_ma_prep_summary)

# Select first row of prior math responses from original dataframe and replace critical columns
# with summary values obtained above
# NB: we manually set 'ma_prep_combined' to have 'experience_math' construct below
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
  rename(
    response_choice = rows,
    response = vals_list
  )

# # sanity checks
# glimpse(pre_course_survey_ma_prep_combined)
# n_distinct(pre_course_survey_ma_prep_combined$class_id, pre_course_survey_ma_prep_combined$student_id)


# Remove ma_prep rows from original dataframe
pre_course_survey_combined = pre_course_survey_combined |> 
  filter(!str_detect(variable_name, 'ma_prep')) |> # should be 54054 - 1715 = 52339
  rbind(pre_course_survey_ma_prep_combined) # should be 52339 +  1132 = 53471 (num rows for each individual student-class id in `pre_course_survey_ma_prep_combined`)
# # sanity checks
# glimpse(pre_course_survey_combined)
# n_distinct(pre_course_survey_combined$class_id, pre_course_survey_combined$student_id)


# ADD CONSTRUCTS ----

# Combine survey and codebook data with "construct" values
pre_course_survey_combined = pre_course_survey_combined |> 
  left_join(construct_lookup, by = c('variable_name' = 'id'))
# # sanity checks
# glimpse(pre_course_survey_combined)
# n_distinct(pre_course_survey_combined$class_id, pre_course_survey_combined$student_id)

# Add 'experience_math' as construct for 'ma_prep_combined' rows manually created above
pre_course_survey_combined$construct[pre_course_survey_combined$variable_name == 'ma_prep_combined'] = 'experience_math'


# Validate the constructs above

# 1. How many unique students do we have (with non-NA construct data)?
pre_course_survey_combined |>
  filter(!is.na(construct)) |> 
  summarize(n_distinct(class_id, student_id))

# 2. When do `prompt_codebook` (from 2023 codebook) and `question` (from 2024 codebook) differ?
# NB: these are all minor wording changes, don't impact the over-arching construct assignment
pre_course_survey_combined |> 
  filter(!is.na(construct)) |> 
  group_by(variable_name, prompt_codebook, question) |>
  filter(prompt_codebook != question) |> 
  summarize(n())


# What survey items do we have constructs for?
# NB: this set exhaustively covers those in planned analyses
pre_course_survey_combined |> 
  filter(!is.na(construct), !is.na(response_choice)) |> 
  group_by(construct, variable_name, prompt_codebook) |>
  summarize(n()) |> 
  print(n = 30)

# What survey items do we *not* have constructs for?
# NB: none of these align with existing constructs or should be added to planned analyses
pre_course_survey_combined |> 
  filter(is.na(construct)) |> 
  group_by(
    construct,
    variable_name, 
    # release, 
    # lrn_question_reference, 
    prompt_codebook
  ) |>
  summarize(n())


# Remove rows with survey items for which we don't assign a top-level construct
pre_course_survey_combined = pre_course_survey_combined |> 
  filter(!is.na(construct))
# # sanity checks
# glimpse(pre_course_survey_combined)
# n_distinct(pre_course_survey_combined$class_id, pre_course_survey_combined$student_id)


# SAVE DATA ----

# Save as .RData
save(pre_course_survey_combined, file = file.path(OUTPUT_PATH, 'processed_college_23_pre_survey.RData'))
# Save as csv
write_csv(pre_course_survey_combined, file.path(OUTPUT_PATH, 'processed_college_23_pre_survey.csv'))



