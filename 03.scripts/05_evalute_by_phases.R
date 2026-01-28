# ---- libraries & setup ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, tidyverse, arrow, scales, purrr)
if (!exists("load_global_utils")) {source(here("03.scripts", "global_utils.R"))}

# ---- load data ----
path_input <- here("02.data", "prep", "CLEANED_ab-test-dataset.parquet")
df_cleaned <- read_parquet(path_input)


# Methods to address multi-variante users:
# 1. Phase 1
df_pase_1 <- 
  df_cleaned%>%
  mutate(new_cohort = cohort) %>%
  filter(event_date >= as.Date("2025-06-02") &
           event_date < as.Date("2025-06-02")+14)

# 2. Phase 2
df_pase_2 <- 
  df_cleaned%>%
  mutate(new_cohort = cohort) %>%
  filter(event_date >= as.Date("2025-06-16") &
           event_date < as.Date("2025-06-16")+14)
  
datasets <- 
  list(
  "1. Phase 1" = df_pase_1,
  "2. Phase 2" = df_pase_2)

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
