
# merge 2024 data

# INIT ----
rm(list = ls())
if(!require("pacman")) install.packages("pacman")
pacman::p_load('tidyverse','here', 'dplyr', 'readr')

here::i_am("analysis/experimental/04_merge_data_sources.R")

DATA_PATH = here::here('data') # top-level directory for data
CK_SURVEY_DATA = file.path('2024_fall_data_processed', 
                           'processed_college_24_pre_survey.csv') 
ENGAGEMENT_DATA = file.path('2024_fall_data_processed', 
                            'processed_college_24_engagement.csv')
EOC_DATA = file.path('2024_fall_data_processed', 
                           'processed_college_24_eoc.csv') 

OUTPUT_PATH = here(DATA_PATH, '2024_fall_data_processed') # directory for writing processed data
# Create the directory if it doesn't exist
if (!dir.exists(OUTPUT_PATH)) {
  dir.create(OUTPUT_PATH, recursive = TRUE)
}

# LOAD DATA ----
# ck presurvey
ck_presurvey_data = read_csv(file.path(DATA_PATH, CK_SURVEY_DATA))

# ck eoc 
eoc_data = read_csv(file.path(DATA_PATH, EOC_DATA))

# ck engagement 
engagement_data = read_csv(file.path(DATA_PATH, ENGAGEMENT_DATA))

# PROCESS & COMBINE COMPATIBLE CK ITEMS ---- 
# 1) combine ck data 
ck_presurvey_data_summary = ck_presurvey_data |>
  group_by(
    release, book,
    institution_id, class_id, student_id,
    construct
  ) |> 
  mutate(
    response_choice = ifelse(response_choice == "null", NA, response_choice),
    response_choice = as.numeric(response_choice)
  ) |> 
  dplyr::summarize(
    construct_mean = mean(response_choice, na.rm = T),
    construct_responses = n()
  ) |>
  ungroup()

# convert survey data summary to wide format
ck_presurvey_data_wide = ck_presurvey_data_summary |>
  pivot_wider(
    id_cols = c(release, book, institution_id, class_id, student_id),
    names_from = c(construct),
    values_from = c(construct_mean, construct_responses)
  )

combined_ck_data = ck_presurvey_data_wide |> 
  # inner join will keep all student survey responses for which we have matched engagement *AND* EOC data in any chapter
  # this is sensible since predictions of engagement with survey values can only use observations 
  # where we have paired engagement data and survey data, while predictions of EOC with engagement *AND* survey values
  # can only use observations where we have EOC data with paired engagement data (by chapter) and survey data
  inner_join(
    # inner join here makes sure we only keep rows where there is matched EOC and engagement data
    # for a given student in a given chapter
    engagement_data |> inner_join(
      #engagement_data |> inner_join(
      eoc_data,
      by = c('release','book', 'institution_id', 'class_id', 'student_id', 'chapter')
    ),
    by = c('release','book','institution_id', 'class_id', 'student_id')
  )

# Save as .RData
save(combined_ck_data, file = file.path(OUTPUT_PATH, 'processed_college_24_ck_combined.Rdata'))
# Save as csv
write_csv(combined_ck_data, file.path(OUTPUT_PATH, 'processed_college_24_ck_combined.csv'))

