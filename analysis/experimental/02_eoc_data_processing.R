
# Data processing script for end of chapter quiz responses

# INIT ----
rm(list = ls())
if(!require("pacman")) install.packages("pacman")
pacman::p_load('tidyverse','here', 'dplyr', 'readr')

here::i_am("analysis/experimental/02_eoc_data_processing.R")

DATA_PATH = here::here('data') # top-level directory for data
RESPONSE_DATA = file.path('2024_fall_clean', 'coursekata', 'raw',
                          'responses.csv') # path to response data file
CODEBOOK_DATA = file.path('2024_fall_clean', 'coursekata','codebooks', 'v5.6-all_survey.csv') # path to codebook data file
CODEBOOK_PAGE_DATA = file.path('2024_fall_clean', 'coursekata','codebooks', 'codebook_page_updated.csv') 

PAGEVIEWS_DATA = file.path('2024_fall_clean', 'coursekata', 'raw',
                           'page_views.csv') # path to pageviews data file
CLASSES_DATA = file.path('2024_fall_clean', 'coursekata', 'raw',
                         'classes.csv') 
OUTPUT_PATH = file.path(DATA_PATH, '2024_fall_data_processed') # directory for writing processed data
# Create the directory if it doesn't exist
if (!dir.exists(OUTPUT_PATH)) {
  dir.create(OUTPUT_PATH, recursive = TRUE)
}

# LOAD DATA ----

# Load raw data from college-24 repository
responses = read_csv(file.path(DATA_PATH, RESPONSE_DATA)) # if csv

# load classes and page views
classes <- read_csv(file.path(DATA_PATH, CLASSES_DATA))
page_views <- read_csv(file.path(DATA_PATH, PAGEVIEWS_DATA)) |>
  left_join(classes, by = c("institution_id", "class_id")) 

responses_codebook <- page_views |>
  select(release, book,chapter, page) |>
  unique() |>
  mutate(chapter_num = as.numeric(gsub("\\D", "", chapter)),
         page_string = str_extract(str_trunc(page, 8, "right"), "\\d+\\.*\\d*"),
         page_num_from_string = as.numeric(str_extract(str_trunc(page, 8, "right"), "\\d+\\.*\\d*")),
         temp_page_within_ch = round((page_num_from_string - chapter_num)/10,3),
         is_10 = grepl(".10", page_string, fixed = TRUE),
         is_between_10_20 = temp_page_within_ch > .010 & temp_page_within_ch < .019,
         page_within_ch = ifelse(is_10 | is_between_10_20, 
                                 temp_page_within_ch*10, 
                                 temp_page_within_ch)) |>
  select(release, book, chapter, page, chapter_num, page_within_ch) |>
  mutate(page_num = page_within_ch*100) |>
  select(-page_within_ch) |>
  filter(!is.na(page_num))

# FILTER DATA ----

# Select only end of chapter review pages from response data
# NB: code in `college-24/scripts/create-checkpoints-eoc.Rmd` keeps *only* first page of review questions
responses_review = responses |>
  filter(
    str_detect(page, 'Review Questions'),
    # removes NA responses from 'plaintext' and 'choicematrix' questions
    lrn_type == 'mcq' # all the 2024 questions are mcq
  ) |> 
  # Trim whitespace on "page" to avoid failure to join with codebook below
  # NB: `page` in responses has value "7.11 Chapter 7 Review Questions " (note space at end) which fails to join w codebook
  mutate(page = str_trim(page))

# COMBINE + PROCESS DATA ----

# Add in codebook information
responses_combined = responses_review |>
  left_join(responses_codebook, by = c('book', 'release', 'chapter', 'page'))

# Summarize performance by chapter
checkpoints_eoc = responses_combined |>
  # Group by book, release, class_id, student_id, chapter_num, page_num
  # i.e., if a student is in two classes, they will appear as two separate students
  # sometimes quizzes are on multiple pages
  group_by(class_id, chapter, chapter_num) |>
  mutate(
    n_total = n_distinct(lrn_question_reference)
  ) |>
  ungroup() |> 
  group_by(book, release, institution_id, class_id, student_id, chapter, chapter_num) |>
  dplyr::summarize(
    # How many questions did the student "submit" an answer to on this page
    questions_submitted = n(),
    # How many questions were answered correctly
    n_correct = sum(points_earned, na.rm = TRUE),
    # # How many total questions were there
    n_total = n_total,
    # Proportion of questions correct divided by total questions on page (both coding and learnosity questions)
    eoc_score = n_correct / n_total,
  ) |>
  ungroup() |>
  distinct()


# SAVE DATA ----
# Save as .RData
save(checkpoints_eoc, file = file.path(OUTPUT_PATH, 'processed_college_24_eoc.RData'))
# Save as csv
write_csv(checkpoints_eoc, file.path(OUTPUT_PATH, 'processed_college_24_eoc.csv'))



