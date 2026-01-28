# ---- libraries & utils ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, tidyverse, janitor, arrow)

if (!exists("load_global_utils")) {source(here("03.scripts", "global_utils.R"))}

# vars ----
path_input  <- here("02.data", "prep", "raw_ab-test-dataset.parquet")
path_output  <- here("02.data", "prep", "CLEANED_ab-test-dataset.parquet")
date_test_start <- as.POSIXct("2025-05-19 00:00:00", tz = "UTC")
date_test_end   <- as.POSIXct("2025-07-14 23:59:59", tz = "UTC")
platform_test <- "android"
threshold_pct <- 80 # percent of user for the steady state of the test
max_gap_tracking_hours <- 2
 
# load data ----
if (file.exists(path_input)) {
  df_raw <- read_parquet(path_input)
} else {stop(paste("file not found: ", path_input))}

# check NA values for all columns ----
colSums(is.na(df_raw))[colSums(is.na(df_raw)) > 0]

## N/A col(action) ----
df_raw %>% group_by(event_name) %>% summarise(sum(!is.na(action)))

df_raw %>% 
  filter(event_name == 'Search_Event' & is.na(action)) %>% 
  group_by(event_name) %>% count()

log_clean(column  = "action",
          result = FALSE,
          rule  = "No NA values",
          action = "KEEP cases. Only event_name = 'Search_Event' have values", 
          cases_removed = 0)

## N/A col(ip_country) ----
df_raw %>% filter(is.na(ip_country))
df_raw %>% filter(is.na(ip_country) & grepl("staging",release_version))

log_clean(column  = "ip_country",
          result = FALSE,
          rule  = "No NA values", 
          action = "REMOVE 44 cases (0.0055%). 21 when release_version = staging",
          cases_removed = 44)

df_raw <- df_raw %>% filter(!is.na(ip_country))

# col(release_version) : only production values ----
df_raw %>% filter(grepl("staging", release_version)) %>% count()

log_clean(column  = "release_version",
          result = FALSE,
          rule  = "Only production version of the app", 
          action = "REMOVE +38 cases. 59 cases total (0.0007%).", 
          cases_removed = 38)

df_raw <- df_raw %>% filter(!grepl("staging", release_version))

# col(event_date): check if all dates in test range.----
is_event_date_valid <-
  all(df_raw$event_date >= date_test_start & 
        df_raw$event_date <= date_test_end)

log_clean(column  = "event_date",
          result = is_event_date_valid,
          rule  = "Dates in scope", 
          action = "",cases_removed = 0)

# col(event_date): check events outliers ----
daily_events <- 
  df_raw %>%
  group_by(event_date) %>%
  summarise(
    n_events = n(),
    n_users = n_distinct(user_id),
    .groups = "drop")

print(daily_events, n=100)

daily_events %>%
  mutate(pct_of_max = n_events / max(n_events) * 100) %>%
  select(event_date, n_events, pct_of_max) %>%
  print(n = 30)

df_steady_state <- 
  daily_events %>%
  mutate(pct_of_max = n_events / max(n_events) * 100) %>%
  filter(pct_of_max >= threshold_pct) %>% # steady state threshold
  summarise(
    start_raw = min(event_date),
    end_raw = max(event_date),
    duration = n(),
    full_weeks = floor(duration / 7),
    final_date = as.Date(start_raw) + (full_weeks * 7 - 1),
    final_duration = full_weeks * 7)

ramp_removed_cases <-
  df_raw %>% 
  filter(
    event_date < as.Date(df_steady_state$start_raw) | 
      event_date > as.Date(df_steady_state$final_date)) %>%
  count()

log_clean(column  = "event_date", 
          result = FALSE, 
          rule  = "No Outliers on a daily basis, no ramp up or ramp down and full weeks", 
          action = "REMOVE 126660 (15.8%). Validate if sample is enough during analysis", 
          cases_removed = ramp_removed_cases)

df_raw <-
  df_raw %>%
  filter (event_date >= as.Date(df_steady_state$start_raw) &
            event_date <=  as.Date(df_steady_state$final_date)) 

# col(event_datetime): event_datetime is aligned with event_date ----
is_datetime_in_same_date <- 
  all(as_date(df_raw$event_datetime) == df_raw$event_date)

df_raw %>%
  filter(as_date(event_datetime) != event_date) %>%
  group_by(ip_country, hour(event_datetime)) %>%
  count()

log_clean(column  = "event_datetime", 
          result = is_datetime_in_same_date, 
          rule  = "25836 rows where > 23h are considered next day", 
          action = "KEEP. But keep an eye on it", 
          cases_removed = 0)

# col(event_datetime): check time continuity for tracking issues ----
## By Cohort ----
gap_cohort <- 
  df_raw %>%
  arrange(cohort, event_datetime) %>%
  group_by(cohort) %>%
  mutate(next_event_datetime = lag(event_datetime),
         time_diff = as.numeric(difftime(event_datetime, next_event_datetime,
                                         units = "hours"))) %>%
  filter(time_diff > max_gap_tracking_hours) %>%
  select(cohort, event_datetime, next_event_datetime, time_diff)

log_clean(column  = "event_datetime", 
          result = FALSE, 
          rule  = "No time gaps in tracking", 
          action = "KEEP. Only 14 cases (0.00175%). Tracking ok by cohort", 
          cases_removed = 0)

## check by app version as there's may exist app release discontinuance ----
gap_cohort_app_version <- 
  df_raw %>%
  arrange(cohort, release_version, event_datetime) %>%
  group_by(cohort, release_version) %>%
  mutate(next_event_datetime = lag(event_datetime),
         time_diff = as.numeric(difftime(event_datetime, next_event_datetime,
                                         units = "hours"))) %>%
  filter(time_diff > max_gap_tracking_hours) %>%
  summarise(gaps_count = sum(!is.na(cohort)))

sum(gap_cohort_app_version$gaps_count)

log_clean(column  = "event_datetime", 
          result = FALSE, 
          rule  = "No time gaps in tracking", 
          action = "KEEP. Only 545 cases in all releases (0.07%). Tracking ok by release version", 
          cases_removed = 0)

# col(platform): Only Android ----
is_android <- 
  all(tolower(df_raw$platform) == platform_test)

log_clean(column  = "platform", 
          result = TRUE, 
          rule  = "Only Android", 
          action = "", 
          cases_removed = 0)

# col(login_status): Only logged in or logged out ----
table(df_raw$login_status)

log_clean(column  = "login_status", 
          result = TRUE, 
          rule  = "Only logged in or logged out", 
          action = "", 
          cases_removed = 0)

# col(user_id) Check UUID format validity ----
uuid_pattern <- "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"

invalid_uuids <- 
  df_raw %>%
  filter(!grepl(uuid_pattern, user_id, ignore.case = TRUE)) %>%
  distinct(user_id)

log_clean(column  = "user_id", 
          result = TRUE, 
          rule  = "All users with UUID valid",
          action = "", 
          cases_removed = 0)

# col(user_id) check for abnormal usage / bots () ----
user_behavior <- 
  df_raw %>%
  group_by(user_id) %>%
  summarise(total_events = n(),
            total_days = n_distinct(event_date)) %>%
  mutate(events_per_day = total_events / total_days) %>%
  arrange(desc(total_events))

log_clean(column  = "user_id", 
          result = FALSE, 
          rule  = "Abnormal usage / bots  ",
          action = "", 
          cases_removed = 0)


# col(user_id) check for duplicate events ----
duplicate_events <- 
  df_raw %>%
  group_by(user_id, event_datetime, event_name) %>%
  filter(n() > 1) %>%
  ungroup()

log_clean(column  = "user_id", 
          result = TRUE, 
          rule  = "Duplicate events",
          action = "REMOVE duplicates events (25 cases 0.003%)", 
          cases_removed = 25)

df_raw <- 
  df_raw %>%
  distinct(user_id, event_datetime, event_name, .keep_all = TRUE)

# col(user_id) users in more than one cohort ----
multi_cohort_users <- 
  df_raw %>%
  group_by(user_id) %>%
  summarise(n_cohorts = n_distinct(cohort)) %>%
  group_by(n_cohorts) %>%
  summarise(qty_users = n_distinct(user_id))

### Contamination:  17.678 (10,17%) of user_ids (app installs) saw 2 variants
### Let's check if it happened in a given day or during all experience

multi_cohort_users_time <- 
  df_raw %>%
  group_by(user_id, event_date) %>%
  summarise(n_cohorts = n_distinct(cohort)) %>%
  group_by(event_date, n_cohorts) %>%
  summarise(qty_users = n_distinct(user_id))

### Contamination trough all experience period. Can't cut.
### Let's check if multi-variant user are similar to uni-variant users.

## ---- Quick comparison 

cohort_comparison <- 
  df_raw %>%
  group_by(user_id) %>%
  summarise(
    is_multi_variant = n_distinct(cohort) > 1,
    total_events = n(),
    search_events = sum(event_name == "Search_Event"),
    days_active = as.numeric(difftime(max(event_datetime), 
                                      min(event_datetime), 
                                      units = "days")) + 1) %>%
  group_by(is_multi_variant) %>%
  summarise(
    n_users = n(),
    avg_events = mean(total_events),
    median_events = median(total_events),
    avg_searches = mean(search_events),
    avg_days = mean(days_active),
    median_days = median(days_active))

cohort_comparison

ratio <- 
  cohort_comparison$avg_events[cohort_comparison$is_multi_variant == TRUE] / 
  cohort_comparison$avg_events[cohort_comparison$is_multi_variant == FALSE]

### Multi-variant users have 2.8x more events than uni-variant users

log_clean(column  = "user_id", 
          result = TRUE, 
          rule  = "Users in Control and Variant. Contamination",
          action = "KEEP cases. Can't remove users as they are significantly 
          different from the others. Evaluate methodolies to avoid discarding 
          test", 
          cases_removed = 0)

# col(login_status) - check that field only allow logged in/out ----
table(df_raw$login_status)

log_clean(column  = "login_status", 
          result = TRUE, 
          rule  = "Only allows logged in/out",
          action = "", 
          cases_removed = 0)

# col(page_type) - Consistent. no misspelling. no noise. ----
table(df_raw$page_type)

log_clean(column  = "page_type", 
          result = TRUE,
          rule  = "Consistent. no misspelling. no noise.",
          action = "", 
          cases_removed = 0)

# col(ip_country) - quick balance check for countries ----
country_check <- 
  df_raw %>%
  distinct(user_id, cohort, ip_country) %>%
  count(ip_country, cohort) %>%
  group_by(ip_country) %>%
  summarise(users=sum(n), var_pct=sum(n[cohort=="Variation"]) /users*100, .groups="drop") %>%
  arrange(desc(users)) %>%
  filter(var_pct < 60)

print(sum(country_check$users) / n_distinct(df_raw$user_id))

log_clean(column  = "ip_country", 
          result = FALSE,
          rule  = "65%/35% split similar in all countries",
          action = "KEEP. It's a language feature, the most important is to check by language", 
          cases_removed = 0)

# col(app_language) - cohort balance by language ----
lang_check <- 
  df_raw %>%
  distinct(user_id, cohort, app_language) %>%
  count(app_language, cohort) %>%
  pivot_wider(names_from = cohort, values_from = n, values_fill = 0) %>%
  mutate(
    total = Control + Variation,
    var_pct = Variation / total * 100) %>%
  arrange(desc(total))

# Heterogeneity test ----
het_data <- 
  df_raw %>%
  filter(event_name == "Search_Event", action == "Search Activated") %>% # primary metric
  count(user_id, app_language, cohort)

het_lang <- 
  anova(lm(n ~ cohort * app_language, data = het_data))["cohort:app_language", "Pr(>F)"]

print(het_lang)

log_clean(
  column = "app_language",
  result = TRUE,
  rule = "Homogeneity across languages",
  action = "KEEP",
  cases_removed = 0)

# create cleaned dataset ----
df_cleaned <-
  df_raw

write_parquet(df_cleaned, path_output)
