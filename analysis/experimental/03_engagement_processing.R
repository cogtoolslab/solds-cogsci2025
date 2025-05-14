
# Engagement calculations for 2024 data

# INIT ----
rm(list = ls())
if(!require("pacman")) install.packages("pacman")
pacman::p_load('tidyverse','here', 'dplyr', 'readr')

here::i_am("analysis/experimental/03_engagement_processing.R")

DATA_PATH = here('data') # top-level directory for data
PAGEVIEWS_DATA = file.path('2024_fall_clean', 'coursekata', 'raw',
                           'page_views.csv') # path to pageviews data file
CLASSES_DATA = file.path('2024_fall_clean', 'coursekata', 'raw',
                         'classes.csv')  

OUTPUT_PATH = file.path(DATA_PATH, '2024_fall_data_processed') # directory for writing processed data


# LOAD DATA ----

# Load raw data
page_views = read_csv(file.path(DATA_PATH, PAGEVIEWS_DATA)) # if csv # loads `page_views` dataframe

# Load codebook data, calculate codebook_pages later 
codebook_classes = read_csv(file.path(DATA_PATH, CLASSES_DATA))

# DATA PROCESSING ----

# Add timestamp formatting to date page was accessed
page_views = page_views |>
  mutate(
    dt_accessed_processed = as.POSIXct(dt_accessed, format = '%Y-%m-%d %H:%M:%OS', tz = 'GMT'),
    dt_accessed_processed_ms = as.numeric(dt_accessed_processed) * 1000,
    # Trim whitespace on "page" to avoid failure to join with codebook below
    # NB: `page` in responses has value "7.11 Chapter 7 Review Questions " (note space at end) which fails to join w codebook
    page = str_trim(page)
  )

# Join with codebooks
page_views_processed = page_views |>
  left_join(
    codebook_classes,
    by = c('institution_id', 'class_id')
  ) 

# CREATE CODEBOOK PAGE
# make a data frame where each row is a page
codebook_pages <- page_views_processed |>
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
  select(release, book, chapter, chapter_num, page, page_within_ch) |>
  mutate(page_num = page_within_ch*100) |>
  select(-page_within_ch) |>
  filter(!is.na(page_num))

write_csv(codebook_pages, file.path(OUTPUT_PATH, 'codebook_page_pageviews.csv'))

page_views_processed = page_views_processed |>
  left_join(
    codebook_pages,
    by = c('release', 'book', 'chapter', 'page')
  )

# keep only non-NA page number rows
page_views_processed = page_views_processed |>
  filter(!is.na(chapter))

# CALCULATE ENGAGEMENT ----

# Proportion of pages the student completes in each chapter over time
student_page_view_summary = page_views_processed |> 
  # filter for completed pages
  filter(was_complete == TRUE) |> 
  # keep only the first appearance (earliest time) of each unique page for each student
  group_by(class_id, student_id, chapter, page) |>
  arrange(dt_accessed_processed, .by_group = TRUE) |>
  distinct(class_id, student_id, chapter, page, .keep_all = TRUE) |>
  ungroup() |> 
  # calculate total unique pages accessed in each class
  group_by(class_id, chapter) |>
  mutate(
    total_unique_pages_accessed_class = n_distinct(page),
  ) |>
  ungroup() |>
  # calculate proportion of pages in each chapter accessed by each student
  group_by(class_id, student_id, chapter) |>
  arrange(dt_accessed_processed, .by_group = TRUE) |>
  mutate(
    total_unique_pages_accessed_student = n_distinct(page),
    pages_so_far = row_number(),
    proportion_pages_accessed = pages_so_far / total_unique_pages_accessed_class
  ) |>
  ungroup()

# Calculate student completion for each page that others in their class completed
# NB: this has separate rows for each page within a chapter, allowing for completion percentage
# by page ("so far"), along with (redundant) rows calculating total completion percentage in each chapter
student_page_view_summary = page_views_processed |>
  # filter for completed pages
  filter(was_complete == TRUE) |>
  # keep only the first appearance (earliest time) of each unique page for each student
  group_by(class_id, student_id, chapter, page) |>
  arrange(dt_accessed_processed, .by_group = TRUE) |>
  distinct(class_id, student_id, chapter, page, .keep_all = TRUE) |>
  ungroup() |>
  # calculate total unique pages accessed in each class
  group_by(class_id, chapter) |>
  mutate(
    total_unique_pages_complete_class = n_distinct(page),
  ) |>
  ungroup() |>
  # calculate proportion of pages in each chapter accessed by each student
  group_by(class_id, student_id, chapter) |>
  arrange(dt_accessed_processed, .by_group = TRUE) |>
  mutate(
    total_unique_pages_complete_student = n_distinct(page),
    pages_complete_so_far = row_number(),
    proportion_pages_complete_so_far = pages_complete_so_far / total_unique_pages_complete_class,
    proportion_pages_complete_student = total_unique_pages_complete_student / total_unique_pages_complete_class
  ) |>
  ungroup()

# Summarize chapter completion rates by student: one row per (student, chapter)
student_page_view_chapter_summary = student_page_view_summary |>
  distinct(
    book, release,
    institution_id, class_id, student_id,
    chapter, chapter,
    total_unique_pages_complete_class, proportion_pages_complete_student,
    proportion_pages_complete_student
  )

# SAVE DATA ----
# Save as .RData
save(student_page_view_chapter_summary, file = file.path(OUTPUT_PATH, 'processed_college_24_engagement.RData'))
# Save as csv
write_csv(student_page_view_chapter_summary, file.path(OUTPUT_PATH, 'processed_college_24_engagement.csv'))

