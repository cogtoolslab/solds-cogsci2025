#
# Data processing script for 2023 engagement (proportion pages complete)
#

# INIT ----
rm(list = ls())

if(!require("pacman")) install.packages("pacman")
pacman::p_load('tidyverse','here', 'dplyr', 'readr')
               
here::i_am("analysis/observational/03_engagement_processing.R")

DATA_PATH = here::here('data') # top-level directory for data
PAGEVIEWS_DATA = file.path('2023-college', 'raw', 'page_views.Rdata') # path to pageviews data file
CLASSES_DATA = file.path('2023-college', 'raw', 'classes.csv') # lookup used for matching institution and class with book and release
PAGE_CODEBOOK_DATA = file.path('2023-college', 'codebooks', 'codebook_page_pageviews.csv') # codebook for matching chapter and page information

OUTPUT_PATH = here('data', '2023-college-processed') # directory for writing processed data
# Create the directory if it doesn't exist
if (!dir.exists(OUTPUT_PATH)) {
  dir.create(OUTPUT_PATH, recursive = TRUE)
}


# LOAD DATA ----

# Load raw data
load(file.path(DATA_PATH, PAGEVIEWS_DATA)) # loads `page_views` dataframe
# # sanity checks
# glimpse(page_views)

# Load codebook data
codebook_classes = read_csv(file.path(DATA_PATH, CLASSES_DATA))
codebook_pages = read_csv(file.path(DATA_PATH, PAGE_CODEBOOK_DATA))[, -1] # remove index column
# # sanity checks
# glimpse(codebook_classes)
# glimpse(codebook_pages)


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
# # sanity checks
# glimpse(page_views)


# Join with codebooks
page_views_processed = page_views |>
  left_join(
    codebook_classes |> select(!n_students),
    by = c('institution_id', 'class_id')
  ) |>
  left_join(
    codebook_pages,
    by = c('release', 'book', 'chapter', 'page')
  )
# # sanity checks
# glimpse(page_views_processed)
# # look at NA-valued chapter and page numbers to make sure they're what we expect:
# # chapter and pages where we do not care about page views or access for engagement purposes
# page_views_processed |>
#   # filter(is.na(chapter_num)) |>
#   filter(is.na(page_num)) |>
#   select(chapter, page) |> distinct()

# keep only non-NA page number rows
page_views_processed = page_views_processed |>
  filter(!is.na(chapter_num))
# # sanity checks
# glimpse(page_views_processed)


# CALCULATE ENGAGEMENT ----

# Proportion of pages the student completes in each chapter over time
student_page_view_summary = page_views_processed |>
  # filter for completed pages
  filter(was_complete == 'true') |>
  # keep only the first appearance (earliest time) of each unique page for each student
  group_by(class_id, student_id, chapter_num, page) |>
  arrange(dt_accessed_processed, .by_group = TRUE) |>
  distinct(class_id, student_id, chapter_num, page, .keep_all = TRUE) |>
  ungroup() |>
  # calculate total unique pages accessed in each class
  group_by(class_id, chapter_num) |>
  mutate(
    total_unique_pages_accessed_class = n_distinct(page),
  ) |>
  ungroup() |>
  # calculate proportion of pages in each chapter accessed by each student
  group_by(class_id, student_id, chapter_num) |>
  arrange(dt_accessed_processed, .by_group = TRUE) |>
  mutate(
    total_unique_pages_accessed_student = n_distinct(page),
    pages_so_far = row_number(),
    proportion_pages_accessed = pages_so_far / total_unique_pages_accessed_class
  ) |>
  ungroup()
# # sanity checks
# glimpse(student_page_view_summary)


# Calculate student completion for each page that others in their class completed
# NB: this has separate rows for each page within a chapter, allowing for completion percentage
# by page ("so far"), along with (redundant) rows calculating total completion percentage in each chapter
student_page_view_summary = page_views_processed |>
  # filter for completed pages
  filter(was_complete == 'true') |>
  # keep only the first appearance (earliest time) of each unique page for each student
  group_by(class_id, student_id, chapter_num, page) |>
  arrange(dt_accessed_processed, .by_group = TRUE) |>
  distinct(class_id, student_id, chapter_num, page, .keep_all = TRUE) |>
  ungroup() |>
  # calculate total unique pages accessed in each class
  group_by(class_id, chapter_num) |>
  mutate(
    total_unique_pages_complete_class = n_distinct(page),
  ) |>
  ungroup() |>
  # calculate proportion of pages in each chapter accessed by each student
  group_by(class_id, student_id, chapter_num) |>
  arrange(dt_accessed_processed, .by_group = TRUE) |>
  mutate(
    total_unique_pages_complete_student = n_distinct(page),
    pages_complete_so_far = row_number(),
    proportion_pages_complete_so_far = pages_complete_so_far / total_unique_pages_complete_class,
    proportion_pages_complete_student = total_unique_pages_complete_student / total_unique_pages_complete_class
  ) |>
  ungroup()
# # sanity checks
# glimpse(student_page_view_summary)


# Summarize chapter completion rates by student: one row per (student, chapter)
student_page_view_chapter_summary = student_page_view_summary |>
  distinct(
    book, release,
    institution_id, class_id, student_id,
    chapter, chapter_num,
    total_unique_pages_complete_class,
    proportion_pages_complete_student
  )
# # sanity checks
# glimpse(student_page_view_chapter_summary)
#   # make sure there is only one row per unique student, chapter
# student_page_view_chapter_summary |> group_by(class_id, student_id, chapter_num) |> summarize(nrows = n()) |> filter(nrows > 1)
#   # are the values sensible?
# summary(student_page_view_chapter_summary$proportion_pages_complete_student)
# unique(student_page_view_chapter_summary$proportion_pages_complete_student)



# SAVE DATA ----

# Save as .RData
save(student_page_view_chapter_summary, file = file.path(OUTPUT_PATH, 'processed_college_23_engagement.RData'))
# Save as csv
write_csv(student_page_view_chapter_summary, file.path(OUTPUT_PATH, 'processed_college_23_engagement.csv'))


# SPLIT-HALF CORRELATION ----

## BOOTSTRAP ----
set.seed(130)
bootstrap_iters = 1000

split_half_corr_bootstrap = replicate(
  n = bootstrap_iters,
  expr = {
    iter_num <- get("i", envir = parent.frame())
    # unique pages completed by class
    unique_class_pages = page_views_processed |>
      # filter for completed pages
      filter(was_complete == 'true') |>
      # keep only the first appearance (earliest time) of each unique page for each student
      group_by(class_id, student_id, chapter_num, page) |> # still coursewise, just grouping by chapter redundantly here 
      arrange(dt_accessed_processed, .by_group = TRUE) |>
      distinct(class_id, student_id,chapter_num, page, .keep_all = TRUE) |>
      ungroup() |> 
      # calculate total unique pages accessed in each class
      group_by(class_id ) |>
      mutate(
        total_unique_pages_accessed_class = n_distinct(page),
      ) |> 
      ungroup() |>
      dplyr::select(class_id, chapter_num, page, total_unique_pages_accessed_class) |>
      distinct(class_id, page, .keep_all = TRUE) |>
      group_by(class_id) |> 
      mutate(
        split = sample(rep(1:2, length.out = n()))
      ) |>
      ungroup() |>
      group_by(class_id, split) |>
      mutate(
        split_total_unique_pages_accessed_class = n_distinct(page),
      ) |> ungroup()
    
    # Proportion of pages the student completes in each chapter over time but split half
    split_half_student_page_view_course = page_views_processed |>
      # filter for completed pages
      filter(was_complete == 'true') |>
      # keep only the first appearance (earliest time) of each unique page for each student
      group_by(class_id, student_id, chapter_num, page) |>
      arrange(dt_accessed_processed, .by_group = TRUE) |>
      distinct(class_id, student_id, chapter_num, page, .keep_all = TRUE) |>
      ungroup() |>
      left_join(unique_class_pages, by = c('class_id', 'chapter_num', 'page')) |> 
      group_by(class_id, split, student_id) |>
      arrange(dt_accessed_processed, .by_group = TRUE) |>
      mutate(
        split_total_unique_pages_complete_student = n_distinct(chapter_num, page),
        # pages_complete_so_far = row_number(),
        # proportion_pages_complete_so_far = pages_complete_so_far / total_unique_pages_complete_class,
        split_proportion_pages_complete_student = split_total_unique_pages_complete_student / split_total_unique_pages_accessed_class
      ) |>
      ungroup() |>
      distinct(class_id, split, student_id, .keep_all = TRUE)
    
    split_half_student_page_view_course_wider = split_half_student_page_view_course |>
      dplyr::select(institution_id,class_id, student_id, split, split_proportion_pages_complete_student) |> 
      pivot_wider(
        names_from = split,  
        values_from = split_proportion_pages_complete_student
      ) 
    
    pearson_corr =  cor.test(split_half_student_page_view_course_wider$`1`, split_half_student_page_view_course_wider$`2`,
                             use = "pairwise.complete.obs")
    spearman_corr =  cor.test(split_half_student_page_view_course_wider$`1`, split_half_student_page_view_course_wider$`2`,
                              method = "spearman", use = "pairwise.complete.obs",exact = FALSE)
    
    
    data.frame(
      iter_num = iter_num,  # Get the current iteration number
      spearman = spearman_corr$estimate,
      pearson = pearson_corr$estimate,
      spearman_brown = (2*pearson_corr$estimate)/(1+pearson_corr$estimate)
    )
  } ,
  simplify = FALSE
)

split_half_corr_bootstrap_df <- bind_rows(split_half_corr_bootstrap)
glimpse(split_half_corr_bootstrap_df)

## SAVE ----
# Save as .RData 
save(split_half_corr_bootstrap_df, file = file.path(OUTPUT_PATH, 'processed_college_23_engagement_splithalf.RData'))
# Save as csv
write_csv(split_half_corr_bootstrap_df, file.path(OUTPUT_PATH, 'processed_college_23_engagement_splithalf.csv'))

## SUMMARY ----
split_half_corr_bootstrap_df_summary = tibble(
  pearson_mean = mean(split_half_corr_bootstrap_df$pearson),
  pearson_lb = quantile(split_half_corr_bootstrap_df$pearson, 0.025),
  pearson_ub = quantile(split_half_corr_bootstrap_df$pearson, 0.975),
  spearman_mean = mean(split_half_corr_bootstrap_df$spearman),
  spearman_lb = quantile(split_half_corr_bootstrap_df$spearman, 0.025),
  spearman_ub = quantile(split_half_corr_bootstrap_df$spearman, 0.975),
  spearman_brown_mean = mean(split_half_corr_bootstrap_df$spearman_brown),
  spearman_brown_lb = quantile(split_half_corr_bootstrap_df$spearman_brown, 0.025),
  spearman_brown_ub = quantile(split_half_corr_bootstrap_df$spearman_brown, 0.975)
)
print(split_half_corr_bootstrap_df_summary)
