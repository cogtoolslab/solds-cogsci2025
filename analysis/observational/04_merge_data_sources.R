#
# Data processing script to merge files
# Merges separate data frames with survey, engagement, and end-of-chapter quiz data
# Performs basic visualization of the combined data (correlation matrix)
# Saves merged data frame for downstream analyses
#

# INIT ----
rm(list = ls())
if(!require("pacman")) install.packages("pacman")
pacman::p_load('tidyverse','here', 'dplyr', 'readr')
here::i_am("analysis/observational/04_merge_data_sources.R")

DATA_PATH = here('data', '2023-college-processed') # top-level directory for data
SURVEY = file.path('processed_college_23_pre_survey.csv')   # CSV file with survey data
EOC = file.path('processed_college_23_eoc.csv') #'processed_college_23_eoc.csv' # CSV file with end of chapter quiz data
ENGAGEMENT = file.path('processed_college_23_engagement.csv') # CSV file with engagement data

# LOAD DATA ----

# survey
survey_data = read_csv(file.path(DATA_PATH, SURVEY))
# glimpse(survey_data) # how many rows?
# n_distinct(survey_data$class_id, survey_data$student_id) # 1438 unique students with survey data

# eoc
eoc_data = read_csv(file.path(DATA_PATH, EOC))
# glimpse(eoc_data) # how many rows?
# n_distinct(eoc_data$class_id, eoc_data$student_id) # 1487 unique students with EOC data

# engagement data
engagement_data = read_csv(file.path(DATA_PATH, ENGAGEMENT))
# glimpse(engagement_data) # how many rows?
# n_distinct(engagement_data$class_id, engagement_data$student_id) # 1571 unique students with engagement data

# SANITY CHECKS ----

# confirm that survey data has one unique row for every (class, student, survey question)
survey_data |>
  group_by(class_id, student_id, variable_name) |>
  summarize(nrows = n()) |>
  filter(nrows > 1)

# confirm that EOC data has one unique row for every (class, student, chapter)
eoc_data |>
  group_by(class_id, student_id, chapter, chapter_num) |>
  summarize(
    nrows = n()
  ) |>
  filter(
    nrows > 1
  )

# confirm that EOC data has no more than 2 unique rows for every (class, student, chapter)
# (many chapters have 2 pgs of review questions)
eoc_data |>
  group_by(class_id, student_id, chapter, chapter_num) |>
  summarize(
    nrows = n()
  ) |>
  filter(
    nrows > 2
  )

# confirm that engagement data has one unique row for every (class, student, chapter)
engagement_data |>
  group_by(class_id, student_id, chapter, chapter_num) |>
  summarize(
    nrows = n()
  ) |>
  filter(
    nrows > 1
  )


# compare students in each dataframe
survey_students = survey_data |>
  select(class_id, student_id) |>
  distinct() |>
  mutate(UID = paste(class_id, student_id, sep = ', ')) |>
  select(UID)
# length(survey_students$UID) # 1438 unique (class, student)

eoc_students = eoc_data |>
  select(class_id, student_id) |>
  distinct() |>
  mutate(UID = paste(class_id, student_id, sep = ', ')) |>
  select(UID)
# length(eoc_students$UID) # 1487 unique (class, student)

engagement_students = engagement_data |> 
  select(class_id, student_id) |>
  distinct() |>
  mutate(UID = paste(class_id, student_id, sep = ', ')) |>
  select(UID)
# length(engagement_students$UID) # 1571 unique (class, student)

# 
# # is survey a subset of EOC?
# all(survey_students$UID %in% eoc_students$UID)
# # is survey a subset of engagement?
# all(survey_students$UID %in% engagement_students$UID)
# # is EOC a subset of engagement?
# all(eoc_students$UID %in% engagement_students$UID)


# # how many students do we have survey data for but no EOC data for?
# length(setdiff(survey_students$UID, eoc_students$UID)) # 124 students we have survey data for but no EOC data
# # sanity check the above
# any(setdiff(survey_students$UID, eoc_students$UID) %in% survey_students$UID)  # TRUE
# any(setdiff(survey_students$UID, eoc_students$UID) %in% eoc_students$UID)     # FALSE
# # how many students do we have EOC data for but no survey data?
# length(setdiff(eoc_students$UID, survey_students$UID)) # 173 students we have EOC data for but no survey data
# # sanity check the above
# any(setdiff(eoc_students$UID, survey_students$UID) %in% eoc_students$UID)     # TRUE
# any(setdiff(eoc_students$UID, survey_students$UID) %in% survey_students$UID)  # FALSE
# # how many students do we have survey data *AND* EOC data for?
# length(intersect(eoc_students$UID, survey_students$UID)) # 1314 students with at least some survey and EOC data
# # sanity check the above
# any(intersect(eoc_students$UID, survey_students$UID) %in% setdiff(eoc_students$UID, survey_students$UID)) # FALSE
# any(intersect(eoc_students$UID, survey_students$UID) %in% setdiff(survey_students$UID, eoc_students$UID)) # FALSE


# # how many students do we have survey data but no engagement data for?
# length(setdiff(survey_students$UID, engagement_students$UID)) # 65 students we have survey data for but no engagement data
# # sanity check the above
# any(setdiff(survey_students$UID, engagement_students$UID) %in% survey_students$UID)     # TRUE
# any(setdiff(survey_students$UID, engagement_students$UID) %in% engagement_students$UID) # FALSE
# # how many students do we have engagement data for but no survey data?
# length(setdiff(engagement_students$UID, survey_students$UID)) # 198 students we have engagement data for but no survey data
# # sanity check the above
# any(setdiff(engagement_students$UID, survey_students$UID) %in% engagement_students$UID) # TRUE
# any(setdiff(engagement_students$UID, survey_students$UID) %in% survey_students$UID)     # FALSE
# # how many students do we have survey data *AND* engagement data for?
# length(intersect(engagement_students$UID, survey_students$UID)) # 1373 students with at least some survey and engagement data
# # sanity check the above
# any(intersect(engagement_students$UID, survey_students$UID) %in% setdiff(engagement_students$UID, survey_students$UID)) # FALSE
# any(intersect(engagement_students$UID, survey_students$UID) %in% setdiff(survey_students$UID, engagement_students$UID)) # FALSE

 
# # how many students do we have EOC data but no engagement data for?
# length(setdiff(eoc_students$UID, engagement_students$UID)) # 8 students we have EOC data for but no engagement data
# # sanity check the above
# any(setdiff(eoc_students$UID, engagement_students$UID) %in% eoc_students$UID)         # TRUE
# any(setdiff(eoc_students$UID, engagement_students$UID) %in% engagement_students$UID)  # FALSE
# # how many students do we have engagement data for but no EOC data?
# length(setdiff(engagement_students$UID, eoc_students$UID)) # 92 students we have engagement data for but no EOC data
# # sanity check the above
# any(setdiff(engagement_students$UID, eoc_students$UID) %in% engagement_students$UID)  # TRUE
# any(setdiff(engagement_students$UID, eoc_students$UID) %in% eoc_students$UID)         # FALSE
# # how many students do we have EOC data *AND* engagement data for?
# length(intersect(engagement_students$UID, eoc_students$UID)) # 1479 students with at least some EOC and engagement data
# # sanity check the above
# any(intersect(engagement_students$UID, eoc_students$UID) %in% setdiff(engagement_students$UID, eoc_students$UID)) # FALSE
# any(intersect(engagement_students$UID, eoc_students$UID) %in% setdiff(eoc_students$UID, engagement_students$UID)) # FALSE

# # how many students do we have survey data *AND* engagement data *AND* EOC data?
# # 1308 students with at least some survey and engagement data and EOC data
# length(intersect(intersect(survey_students$UID, engagement_students$UID), eoc_students$UID))

# MERGE DATA ----

# get survey construct averages and convert to wide format for merging
survey_data_summary = survey_data |>
  group_by(
    release, book,
    institution_id, class_id, student_id,
    construct
  ) |>
  summarize(
    construct_mean = mean(response_choice, na.rm = T),
    construct_responses = n()
  ) |>
  ungroup()
# # sanity checks
# glimpse(survey_data_summary)
# # confirm we have one row per (class, student, construct)
# survey_data_summary |> group_by(class_id, student_id, construct) |> summarize(nrows=n()) |> filter(nrows > 1)

# convert survey data summary to wide format
survey_data_wide = survey_data_summary |>
  pivot_wider(
    id_cols = c(release, book, institution_id, class_id, student_id),
    names_from = c(construct),
    values_from = c(construct_mean, construct_responses)
  )
# # sanity checks
# glimpse(survey_data_wide)
# n_distinct(survey_data_wide$class_id, survey_data_wide$student_id)

# merge survey, engagement, and EOC data
combined_data = survey_data_wide |> 
  # inner join will keep all student survey responses for which we have matched engagement *AND* EOC data in any chapter
  # this is sensible since predictions of engagement with survey values can only use observations 
  # where we have paired engagement data and survey data, while predictions of EOC with engagement *AND* survey values
  # can only use observations where we have EOC data with paired engagement data (by chapter) and survey data
  inner_join(
    # inner join here makes sure we only keep rows where there is matched EOC and engagement data
    # for a given student in a given chapter
    engagement_data |> inner_join(
      eoc_data,
      by = c('release', 'book', 'institution_id', 'class_id', 'student_id', 'chapter', 'chapter_num')
    ),
    by = c('release', 'book', 'institution_id', 'class_id', 'student_id')
  )
# 
# # sanity checks
# glimpse(combined_data)
# # how many students do we have represented across *all three* datasets?
# n_distinct(combined_data$class_id, combined_data$student_id)


# SAVE DATA ----

# Save as .RData
save(combined_data, file = file.path(DATA_PATH, 'processed_college_23_combined.RData'))
# Save as csv
write_csv(combined_data, file.path(DATA_PATH, 'processed_college_23_combined.csv'))



