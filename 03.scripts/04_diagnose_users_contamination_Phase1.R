# Diagnose multi-variante users 

# ---- libraries & setup ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, tidyverse, arrow, scales, purrr)
if (!exists("load_global_utils")) {source(here("03.scripts", "global_utils.R"))}

# ---- load data ----
path_input_raw <- here("02.data", "prep", "raw_ab-test-dataset.parquet")
path_input <- here("02.data", "prep", "CLEANED_ab-test-dataset.parquet")
phase1_start_date <- as.Date("2025-06-02")
phase1_end_date <- as.Date("2025-06-02")+14
df_cleaned <- read_parquet(path_input)


# Methods to address multi-variante users:
# 0. Raw
df_raw <- 
  read_parquet(path_input_raw) %>%
  mutate(new_cohort = cohort) %>%
  filter(event_date >= phase1_start_date &
           event_date < phase1_end_date)
  
# 1. Original (Session-Level / As-Treated) - Cleaned
df_cleaned <-
  df_cleaned %>%
  mutate(new_cohort = cohort) %>%
  filter(event_date >= phase1_start_date &
           event_date < phase1_end_date)

df_original <-
  df_cleaned

# 2. Remove Swithers
switchers_users <-
  df_cleaned %>%
  group_by(user_id) %>%
  summarise(qty_cohorts = n_distinct(cohort)) %>%
  arrange(desc(qty_cohorts)) %>%
  filter(qty_cohorts > 1) %>%
  pull(user_id)

df_no_switchers <-
  df_cleaned %>%
  filter(!(user_id %in% switchers_users)) %>%
  mutate(new_cohort = cohort)

# 3. Intent to Treat (assign to first variant event)
df_itt <- 
  df_cleaned %>%
  group_by(user_id) %>%
  arrange(event_datetime) %>%
  mutate(new_cohort = first(cohort)) %>%
  ungroup()

# 4. Majority vote (assign to user where he had more events)
df_majority <- 
  df_cleaned %>%
  add_count(user_id, cohort, name = "n_in_cohort") %>%
  group_by(user_id) %>%
  arrange(desc(n_in_cohort), cohort) %>%
  mutate(new_cohort = first(cohort)) %>%
  ungroup()

# 5. Last Exposure (assign user to last event)
df_last <- 
  df_cleaned %>%
  group_by(user_id) %>%
  arrange(event_datetime) %>%
  mutate(new_cohort = last(cohort)) %>%
  ungroup()

# 6. Recency-Weighted (weight events by their recency (later = more weight))
df_recency <- 
  df_cleaned %>%
  group_by(user_id) %>%
  arrange(event_datetime) %>%
  mutate(
    weight = row_number() / n(),
    cohort_num = as.numeric(cohort == "Variation")
  ) %>%
  mutate(new_cohort = ifelse(sum(weight * cohort_num) > 0.5, "Variation", "Control")) %>%
  ungroup()

# 7. Without Time Bias ----
df_no_time_bias <- 
  df_cleaned %>%
  filter(event_date >= as.Date("2025-05-30"),
         event_date < as.Date("2025-06-15"))%>%
  mutate(new_cohort = cohort)

datasets <- 
  list(
  "0. Raw" = df_raw,
  "1. Original" = df_original,
  "2. No Switchers" = df_no_switchers,
  "3. Intent To Treat" = df_itt,
  "4. Majority" = df_majority,
  "5. Last" = df_last,
  "6. Recency" = df_recency,
  "7. No Time Bias" = df_no_time_bias)


# percentage of Searcher in each cohort
searcher_rate_results <- 
  map_df(
  names(datasets),
  ~calculate_searcher_rate(datasets[[.x]], "new_cohort", .x))

# Searches per Searcher ----
searches_per_searcher <- 
  map_df(
    names(datasets),
    ~calculate_searches_per_searcher(datasets[[.x]], "new_cohort", .x))

# Search Activation to Search Results Conversion ----
search_activation_to_results <- 
  map_df(
    names(datasets),
    ~calculate_search_activation_to_results(datasets[[.x]], "new_cohort", .x))

# search_to_property_conversion ----
search_to_property_conversion <- 
  map_df(
    names(datasets),
    ~calculate_search_to_property_conversion(datasets[[.x]], "new_cohort", .x))

# calculate_booking_conversion ----
booking_conversion <- 
  map_df(
    names(datasets),
    ~calculate_booking_conversion(datasets[[.x]], "new_cohort", .x))

result <- 
  bind_rows(
  searches_per_searcher,
  search_activation_to_results,
  search_to_property_conversion,
  booking_conversion,
  searcher_rate_results)
