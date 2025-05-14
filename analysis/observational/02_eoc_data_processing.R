#
# Data processing script for 2023 end of chapter quiz responses
#

# INIT ----
rm(list = ls())
if(!require("pacman")) install.packages("pacman")
pacman::p_load('tidyverse','here', 'dplyr', 'readr')

here::i_am("analysis/observational/02_eoc_data_processing.R")

DATA_PATH = here::here('data') # top-level directory for data
RESPONSE_DATA = file.path('2023-college', 'raw', 'responses.Rdata') # path to response data file

CODEBOOK_DATA = file.path('2023-college', 'codebooks','codebook_page.csv') # path to codebook file from 2023-college public repository

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


# Load codebook for interpreting end of chapter responses
# NB: this is sourced from outside the coursekata 2023-college repository
# and is instead part of the coursekata internal-research repository (internal-research/codebooks)
responses_codebook = read_csv(file.path(DATA_PATH, CODEBOOK_DATA))[,-1]
# # sanity checks
# glimpse(responses_codebook)

# FILTER DATA ----

# Select only end of chapter review pages from response data
# NB: code in `2023-college/scripts/create-checkpoints-eoc.Rmd` keeps *only* first page of review questions
responses_review = responses |>
  filter(
    str_detect(page, 'Review')
  ) |> 
  # Trim whitespace on "page" to avoid failure to join with codebook below
  # NB: `page` in responses has value "7.11 Chapter 7 Review Questions " (note space at end) which fails to join w codebook
  mutate(page = str_trim(page))
# #view(responses_review)
# unique(responses_review$chapter) # 1-9

responses_review = responses |>
  filter(
    str_detect(page, 'Review Questions'),
    # removes NA responses from 'plaintext' and 'choicematrix' questions
    # NB: this removes a small set of questions from chapter 8 that were type 'plaintext' but *do* have a non-NA `points_possible`
    # but without validating these score assignments ourselves, safest thing is to just consider MCQ here
    lrn_type == 'mcq'
  ) |> 
  # Trim whitespace on "page" to avoid failure to join with codebook below
  # NB: `page` in responses has value "7.11 Chapter 7 Review Questions " (note space at end) which fails to join w codebook
  mutate(page = str_trim(page))
# # sanity checks
# glimpse(responses_review)
# unique(responses_review$page)
# unique(responses_review$item_id)
# n_distinct(responses_review$student_id)
# table(responses_review$lrn_type)

# # check for NA responses
#   # points_earned
# unique(responses_review$points_earned)
# table(responses_review$points_earned)
# sum(is.na(responses_review$points_earned))
#   # points_possible
# unique(responses_review$points_possible)
# table(responses_review$points_possible)
# sum(is.na(responses_review$points_possible))



# COMBINE + PROCESS DATA ----

# Add in codebook information to get chapter_num
responses_combined = responses_review |>
  left_join(responses_codebook |> dplyr::select('book', 'release', 'chapter','chapter_num','page'), by = c('book', 'release', 'chapter', 'page'))
# # sanity checks
# glimpse(responses_combined)
# n_distinct(responses_combined$class_id, responses_combined$student_id)

# summarize number of question types by chapater (some have multiple pages of eoc)
eoc_responses_codebook = responses_codebook |>
  filter(
    str_detect(page, 'Review')
  ) |> 
  dplyr::select(-page, -page_num) |>
  group_by(book, release, chapter, chapter_num) |>
  dplyr::summarize(across(everything(), sum, na.rm = TRUE), .groups = "drop")

# Summarize performance by chapter
checkpoints_eoc = responses_combined |>
  # Group by book, release, class_id, student_id, chapter_num, page_num
  # combine if there are multiple pages per eoc in a chapter 
  # i.e., if a student is in two classes, they will appear as two separate students
  group_by(book, release, institution_id, class_id, student_id, chapter, chapter_num) |>
  dplyr::summarize(
    # How many questions did the student "submit" an answer to on this page
    questions_submitted = n(),
    # How many questions were answered correctly
    n_correct = sum(points_earned, na.rm = TRUE),
    # How many total questions were there
    n_attempted = sum(points_possible, na.rm = TRUE)
  ) |>
  ungroup() |>
  left_join(eoc_responses_codebook |> dplyr::select(book, release, chapter,chapter_num, mcq), by = c('book', 'release', 'chapter','chapter_num')) |> 
  dplyr::rename(
    n_total = mcq
  ) |>
  group_by(book, release, institution_id, class_id, student_id, chapter,chapter_num) |>
  mutate(
    # Proportion of questions correct divided by total questions on page (both coding and learnosity questions)
    eoc_score = n_correct / n_total
  ) |>
  ungroup()

# # sanity checks
# glimpse(checkpoints_eoc)
# 
# # at (class_id, student_id, chapter) granularity, each row is a unique question response (see `points_possible`, `points_earned` vals)
# # some chapters have multiple pages 
# responses_combined |> 
#   filter(
#     class_id == '25d5269a-ea76-4cb4-844d-3dbcf0bffc53', 
#     student_id == '0d75f8e3-316d-4bdc-98fe-a3848c2a2e81', 
#     chapter_num == "1"
#   ) |> 
#   glimpse()
# 
# # summary of the above
# checkpoints_eoc |> 
#   filter(
#     class_id == '25d5269a-ea76-4cb4-844d-3dbcf0bffc53', 
#     student_id == '0d75f8e3-316d-4bdc-98fe-a3848c2a2e81',
#     chapter_num == "1"
#   ) |> 
#   glimpse()
# 
# unique(checkpoints_eoc$book)
# unique(checkpoints_eoc$release)
# unique(checkpoints_eoc$class_id)
# length(unique(checkpoints_eoc$student_id))
# unique(checkpoints_eoc$chapter)
# unique(checkpoints_eoc$chapter_num)
# #unique(checkpoints_eoc$page)



# SAVE DATA ----

# Save as .RData
save(checkpoints_eoc, file = file.path(OUTPUT_PATH, 'processed_college_23_eoc.RData'))
# Save as csv
write_csv(checkpoints_eoc, file.path(OUTPUT_PATH, 'processed_college_23_eoc.csv'))

