# Diagnose Homogeneitey

# ---- libraries & setup ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, tidyverse, arrow, scales, purrr)
if (!exists("load_global_utils")) {source(here("03.scripts", "global_utils.R"))}

# ---- load data ----
path_input_raw <- here("02.data", "prep", "raw_ab-test-dataset.parquet")
path_input <- here("02.data", "prep", "CLEANED_ab-test-dataset.parquet")
df_cleaned <- read_parquet(path_input)


# Methods to address multi-variante users:
# 0. Raw
df_raw <- 
  read_parquet(path_input_raw) %>%
  mutate(new_cohort = cohort)

# 1. Original (Session-Level / As-Treated) - Cleaned
df_original <-
  df_cleaned %>%
  mutate(new_cohort = cohort)

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

datasets <- 
  list(
    "0. Raw" = df_raw,
    "1. Original" = df_original,
    "2. No Switchers" = df_no_switchers,
    "3. Intent To Treat" = df_itt,
    "4. Majority" = df_majority,
    "5. Last" = df_last,
    "6. Recency" = df_recency)


# Test temporal homogeneity for all methods
temporal_homogeneity_results <- map_df(
  names(datasets),
  ~test_temporal_homogeneity(datasets[[.x]], "cohort", .x))

print(temporal_homogeneity_results, n = 20)



# Calculate daily events by cohort with percentages
daily_events_pct <- 
  map_df(names(datasets), function(method_name) {
  datasets[[method_name]] %>%
    mutate(date = as.Date(event_date)) %>%
    count(date, cohort) %>%
    group_by(date) %>%
    mutate(
      total_daily = sum(n),
      pct = n / total_daily * 100
    ) %>%
    ungroup() %>%
    mutate(method = method_name)
})



p <- ggplot(daily_events_pct, aes(x = date, y = pct, color = cohort, group = cohort)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_point(size = 1, alpha = 0.5) +
  facet_wrap(~ method, ncol = 2) +
  scale_color_manual(
    values = c("Control" = "#FF6B6B", "Variation" = "#4ECDC4")
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 100),
    breaks = seq(0, 100, 25)
  ) +
  labs(
    title = "Daily Event Allocation by Cohort - All Methods",
    subtitle = "Percentage of daily events per cohort over time",
    x = "Date",
    y = "% of Daily Events",
    color = "Cohort"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    strip.text = element_text(size = 11, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.title = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(size = 0.3, color = "gray90")
  )

ggsave(
  here("08.output", "daily_events_pct_lines_all_methods.png"),
  plot = p,
  width = 16,
  height = 9,
  units = "in",
  dpi = 300)