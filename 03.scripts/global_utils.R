### NEED re-factoring to re-use tests


### ---- Utils Global File ----

# ---- libraries & utils ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, pwr)

load_global_utils <- 
  function() {
    print("Loading Global Utils...")}

log_clean <- function(result, column, rule, action = "", cases_removed,
                      file = here("02.data", "prep", "data_clean_log.tsv")) {
  
    cat(sprintf("[%s] Column(%s) -> Rule: %s | Action: %s\n | Cases Removed: %s\n", 
                result, column, rule, action, cases_removed))

  if (!file.exists(file)) {
    write("Column\tRule\tStatus\tAction\tCases Removed", file = file)
  }
  line <- sprintf('"%s"\t"%s"\t"%s"\t"%s"\t"%s"', column, rule, result, action, cases_removed)
  write(line, file = file, append = TRUE)
}

# FUNCTION: Searchers percentage ----
calculate_searcher_rate <- 
  function(df_events, cohort_col = "new_cohort", method_name = "Unknown") {
    
    # User-level: is_searcher?
    user_level <- df_events %>%
      group_by(user_id, !!sym(cohort_col)) %>%
      summarise(is_searcher = any(event_name == "Search_Event" & 
                                    action == "Search Activated" & 
                                    page_type == "Destination Search"),
                .groups = "drop")
    
    # Aggregate by cohort
    cohort_stats <- user_level %>%
      group_by(!!sym(cohort_col)) %>%
      summarise(n = n(), searchers = sum(is_searcher), rate = searchers/n, .groups = "drop")
    
    # Extract values
    control <- cohort_stats %>% filter(!!sym(cohort_col) == "Control")
    variation <- cohort_stats %>% filter(!!sym(cohort_col) == "Variation")
    
    n1 <- control$n
    n2 <- variation$n
    p1 <- control$rate
    p2 <- variation$rate
    
    # Chi-square test
    contingency <- matrix(c(control$searchers, n1 - control$searchers,
                            variation$searchers, n2 - variation$searchers),
                          nrow = 2, byrow = TRUE)
    chi_test <- chisq.test(contingency)
    
    # Effect size (Cohen's h)
    h <- 2 * (asin(sqrt(p2)) - asin(sqrt(p1)))
    
    # Harmonic mean sample size
    n_harmonic <- (2 * n1 * n2) / (n1 + n2)
    
    # Power analysis
    power_result <- pwr.2p.test(
      h = h,
      n = n_harmonic,
      sig.level = 0.05,
      alternative = "two.sided"
    )
    
    # Results
    tibble(
      method = method_name,
      metric = "AUX: Searchers percentage",
      control = round(p1 * 100, 2),
      variation = round(p2 * 100, 2),
      lift_pct = round(((p2 - p1) / p1) * 100, 1),
      p_value = ifelse(chi_test$p.value < 0.001, "<0.001", sprintf("%.4f", chi_test$p.value)),
      power = round(power_result$power * 100, 1),
      significant = chi_test$p.value < 0.05,
      well_powered = power_result$power >= 0.8)
  }


# FUNCTION: Primary Metric Searches Per Searcher ----
calculate_searches_per_searcher <- 
  function(df_events, cohort_col = "new_cohort", method_name = "Unknown") {
  
  # User-level: count searches and identify searchers
  user_level <- df_events %>%
    group_by(user_id, !!sym(cohort_col)) %>%
    summarise(
      n_searches = sum(event_name == "Search_Event" & 
                         action == "Search Activated" & 
                         page_type == "Destination Search"),
      is_searcher = n_searches > 0,
      .groups = "drop"
    )
  
  # Filter only searchers
  searchers_only <- user_level %>% filter(is_searcher)
  
  # Split by cohort
  control_searches <- searchers_only %>% 
    filter(!!sym(cohort_col) == "Control") %>% 
    pull(n_searches)
  
  variation_searches <- searchers_only %>% 
    filter(!!sym(cohort_col) == "Variation") %>% 
    pull(n_searches)
  
  # Check if we have data
  if (length(control_searches) == 0 | length(variation_searches) == 0) {
    return(tibble(
      method = method_name,
      metric = "PRI: Searches per searcher",
      control = NA_real_,
      variation = NA_real_,
      lift_pct = NA_real_,
      p_value = "NA",
      power = NA_real_,
      significant = NA,
      well_powered = NA
    ))
  }
  
  # Calculate statistics
  n1 <- length(control_searches)
  n2 <- length(variation_searches)
  mean1 <- mean(control_searches)
  mean2 <- mean(variation_searches)
  sd1 <- sd(control_searches)
  sd2 <- sd(variation_searches)
  
  # Welch's t-test (handles unequal variances)
  t_test <- t.test(
    control_searches,
    variation_searches,
    alternative = "two.sided"
  )
  
  # Effect size (Cohen's d)
  pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
  cohens_d <- abs((mean2 - mean1) / pooled_sd)
  
  # Power analysis for t-test
  power_result <- tryCatch({
    pwr.t2n.test(
      n1 = n1,
      n2 = n2,
      d = cohens_d,
      sig.level = 0.05,
      alternative = "two.sided"
    )$power
  }, error = function(e) {
    # Fallback: use equal n approximation
    n_harmonic <- (2 * n1 * n2) / (n1 + n2)
    pwr.t.test(
      n = n_harmonic,
      d = cohens_d,
      sig.level = 0.05,
      type = "two.sample",
      alternative = "two.sided"
    )$power
  })
  
  # Results
  tibble(
    method = method_name,
    metric = "PRI: Searches per searcher",
    control = round(mean1, 2),
    variation = round(mean2, 2),
    lift_pct = round(((mean2 - mean1) / mean1) * 100, 1),
    p_value = ifelse(t_test$p.value < 0.001, "<0.001", sprintf("%.4f", t_test$p.value)),
    power = round(power_result * 100, 1),
    significant = t_test$p.value < 0.05,
    well_powered = power_result >= 0.8
  )
  }

# FUNCTION: Health Metric: Search Activation to Search Results Conversion ----
calculate_search_activation_to_results <- 
  function(df_events, cohort_col = "new_cohort", method_name = "Unknown") {
  
  # User-level: check if next event after search submitted is viewing search page
  user_level <- df_events %>%
    arrange(user_id, event_datetime) %>%
    group_by(user_id, !!sym(cohort_col)) %>%
    mutate(
      # Flag search submitted
      is_search_submitted = event_name == "Search_Event" & 
        action == "Search Submitted" & 
        page_type == "Destination Search",
      # Flag search page viewed
      is_search_page_viewed = event_name == "Destination_Search_Page_Viewed",
      # Next event is search page viewed?
      next_is_search_page = lead(is_search_page_viewed, default = FALSE)
    ) %>%
    summarise(
      # Did they submit search?
      submitted_search = any(is_search_submitted),
      # Did search page view come immediately after submission?
      viewed_after = any(is_search_submitted & next_is_search_page),
      .groups = "drop"
    )
  
  # Filter only users who submitted search
  submitters_only <- user_level %>% filter(submitted_search)
  
  # Aggregate by cohort
  cohort_stats <- submitters_only %>%
    group_by(!!sym(cohort_col)) %>%
    summarise(
      n_submitters = n(),
      n_converters = sum(viewed_after),
      conversion_rate = n_converters / n_submitters,
      .groups = "drop"
    )
  
  # Extract values
  control <- cohort_stats %>% filter(!!sym(cohort_col) == "Control")
  variation <- cohort_stats %>% filter(!!sym(cohort_col) == "Variation")
  
  # Check if we have data
  if (nrow(control) == 0 | nrow(variation) == 0) {
    return(tibble(
      method = method_name,
      metric = "H: Search activation to results conversion",
      control = NA_real_,
      variation = NA_real_,
      lift_pct = NA_real_,
      p_value = "NA",
      power = NA_real_,
      significant = NA,
      well_powered = NA
    ))
  }
  
  n1 <- control$n_submitters
  n2 <- variation$n_submitters
  p1 <- control$conversion_rate
  p2 <- variation$conversion_rate
  
  # Chi-square test
  contingency <- matrix(c(control$n_converters, n1 - control$n_converters,
                          variation$n_converters, n2 - variation$n_converters),
                        nrow = 2, byrow = TRUE)
  chi_test <- chisq.test(contingency)
  
  # Effect size (Cohen's h)
  h <- 2 * (asin(sqrt(p2)) - asin(sqrt(p1)))
  
  # Harmonic mean sample size
  n_harmonic <- (2 * n1 * n2) / (n1 + n2)
  
  # Power analysis
  power_result <- pwr.2p.test(
    h = h,
    n = n_harmonic,
    sig.level = 0.05,
    alternative = "two.sided"
  )
  
  # Results
  tibble(
    method = method_name,
    metric = "H: Search activation to results conversion",
    control = round(p1 * 100, 2),
    variation = round(p2 * 100, 2),
    lift_pct = round(((p2 - p1) / p1) * 100, 1),
    p_value = ifelse(chi_test$p.value < 0.001, "<0.001", sprintf("%.4f", chi_test$p.value)),
    power = round(power_result$power * 100, 1),
    significant = chi_test$p.value < 0.05,
    well_powered = power_result$power >= 0.8
  )
}

# FUNCTION: Health Metric: Search Activation to Property Page Conversion ----
calculate_search_to_property_conversion <- 
  function(df_events, cohort_col = "new_cohort", method_name = "Unknown") {
  
  # User-level: check if they viewed property page after ANY search activation
  user_level <- df_events %>%
    arrange(user_id, event_datetime) %>%
    group_by(user_id, !!sym(cohort_col)) %>%
    summarise(
      # All search activation times
      search_times = list(event_datetime[event_name == "Search_Event" & 
                                           action == "Search Activated" & 
                                           page_type == "Destination Search"]),
      # All property view times (CORRECTED)
      property_times = list(event_datetime[event_name == "view_item" & 
                                             page_type == "Property Details"]),
      .groups = "drop"
    ) %>%
    mutate(
      # Did they activate search?
      activated_search = lengths(search_times) > 0,
      # Did they view property AFTER any search activation?
      viewed_property = activated_search & 
        lengths(property_times) > 0 &
        any(outer(unlist(property_times), unlist(search_times), `>`))
    )
  
  # Filter only users who activated search
  activators_only <- user_level %>% filter(activated_search)
  
  # Aggregate by cohort
  cohort_stats <- activators_only %>%
    group_by(!!sym(cohort_col)) %>%
    summarise(
      n_activators = n(),
      n_converters = sum(viewed_property),
      conversion_rate = n_converters / n_activators,
      .groups = "drop"
    )
  
  # Extract values
  control <- cohort_stats %>% filter(!!sym(cohort_col) == "Control")
  variation <- cohort_stats %>% filter(!!sym(cohort_col) == "Variation")
  
  # Check if we have data
  if (nrow(control) == 0 | nrow(variation) == 0) {
    return(tibble(
      method = method_name,
      metric = "H: Search activation to property page conversion",
      control = NA_real_,
      variation = NA_real_,
      lift_pct = NA_real_,
      p_value = "NA",
      power = NA_real_,
      significant = NA,
      well_powered = NA
    ))
  }
  
  n1 <- control$n_activators
  n2 <- variation$n_activators
  p1 <- control$conversion_rate
  p2 <- variation$conversion_rate
  
  # Chi-square test
  contingency <- matrix(c(control$n_converters, n1 - control$n_converters,
                          variation$n_converters, n2 - variation$n_converters),
                        nrow = 2, byrow = TRUE)
  chi_test <- chisq.test(contingency)
  
  # Effect size (Cohen's h)
  h <- 2 * (asin(sqrt(p2)) - asin(sqrt(p1)))
  
  # Harmonic mean sample size
  n_harmonic <- (2 * n1 * n2) / (n1 + n2)
  
  # Power analysis
  power_result <- pwr.2p.test(
    h = h,
    n = n_harmonic,
    sig.level = 0.05,
    alternative = "two.sided"
  )
  
  # Results
  tibble(
    method = method_name,
    metric = "H: Search activation to property page conversion",
    control = round(p1 * 100, 2),
    variation = round(p2 * 100, 2),
    lift_pct = round(((p2 - p1) / p1) * 100, 1),
    p_value = ifelse(chi_test$p.value < 0.001, "<0.001", sprintf("%.4f", chi_test$p.value)),
    power = round(power_result$power * 100, 1),
    significant = chi_test$p.value < 0.05,
    well_powered = power_result$power >= 0.8
  )
}

# FUNCTION: Health Metric: Booking Conversion Rate ----
calculate_booking_conversion <- 
  function(df_events, cohort_col = "new_cohort", method_name = "Unknown") {
  
  # User-level: did they complete a booking?
  user_level <- df_events %>%
    group_by(user_id, !!sym(cohort_col)) %>%
    summarise(
      # Did user complete a booking?
      made_booking = any(event_name == "purchase"),
      .groups = "drop"
    )
  
  # Aggregate by cohort
  cohort_stats <- user_level %>%
    group_by(!!sym(cohort_col)) %>%
    summarise(
      n_users = n(),
      n_bookers = sum(made_booking),
      conversion_rate = n_bookers / n_users,
      .groups = "drop"
    )
  
  # Extract values
  control <- cohort_stats %>% filter(!!sym(cohort_col) == "Control")
  variation <- cohort_stats %>% filter(!!sym(cohort_col) == "Variation")
  
  # Check if we have data
  if (nrow(control) == 0 | nrow(variation) == 0) {
    return(tibble(
      method = method_name,
      metric = "H: Booking conversion rate",
      control = NA_real_,
      variation = NA_real_,
      lift_pct = NA_real_,
      p_value = "NA",
      power = NA_real_,
      significant = NA,
      well_powered = NA
    ))
  }
  
  n1 <- control$n_users
  n2 <- variation$n_users
  p1 <- control$conversion_rate
  p2 <- variation$conversion_rate
  
  # Chi-square test
  contingency <- matrix(c(control$n_bookers, n1 - control$n_bookers,
                          variation$n_bookers, n2 - variation$n_bookers),
                        nrow = 2, byrow = TRUE)
  chi_test <- chisq.test(contingency)
  
  # Effect size (Cohen's h)
  h <- 2 * (asin(sqrt(p2)) - asin(sqrt(p1)))
  
  # Harmonic mean sample size
  n_harmonic <- (2 * n1 * n2) / (n1 + n2)
  
  # Power analysis
  power_result <- pwr.2p.test(
    h = h,
    n = n_harmonic,
    sig.level = 0.05,
    alternative = "two.sided"
  )
  
  # Results
  tibble(
    method = method_name,
    metric = "H: Booking conversion rate",
    control = round(p1 * 100, 2),
    variation = round(p2 * 100, 2),
    lift_pct = round(((p2 - p1) / p1) * 100, 1),
    p_value = ifelse(chi_test$p.value < 0.001, "<0.001", sprintf("%.4f", chi_test$p.value)),
    power = round(power_result$power * 100, 1),
    significant = chi_test$p.value < 0.05,
    well_powered = power_result$power >= 0.8
  )
  }



test_temporal_homogeneity <- 
  function(df, cohort_col = "cohort", method_name = "Unknown") {
  # Get first event per user (user-level)
  df_users <- df %>%
    group_by(user_id) %>%
    arrange(event_datetime) %>%
    slice(1) %>%
    ungroup() %>%
    select(user_id, !!sym(cohort_col), event_date, event_datetime)
  
  # Check if we have both cohorts
  cohorts <- unique(df_users[[cohort_col]])
  
  if (length(cohorts) < 2) {
    return(tibble(
      method = method_name,
      metric = c("event_date", "event_datetime"),
      ks_statistic = NA,
      p_value = NA,
      homogeneous = NA,
      control_median = NA,
      variation_median = NA,
      note = "Only one cohort"
    ))
  }
  
  # Split by cohort
  control_dates <- df_users %>%
    filter(!!sym(cohort_col) == "Control") %>%
    pull(event_datetime) %>%
    as.numeric()
  
  variation_dates <- df_users %>%
    filter(!!sym(cohort_col) == "Variation") %>%
    pull(event_datetime) %>%
    as.numeric()
  
  # Check if we have data
  if (length(control_dates) == 0 || length(variation_dates) == 0) {
    return(tibble(
      method = method_name,
      metric = c("event_date", "event_datetime"),
      ks_statistic = NA,
      p_value = NA,
      homogeneous = NA,
      control_median = NA,
      variation_median = NA,
      note = "No data for one cohort"
    ))
  }
  
  # Kolmogorov-Smirnov test
  ks_test <- ks.test(control_dates, variation_dates)
  
  # Median dates for comparison
  control_median <- df_users %>%
    filter(!!sym(cohort_col) == "Control") %>%
    summarise(median = median(event_datetime)) %>%
    pull(median)
  
  variation_median <- df_users %>%
    filter(!!sym(cohort_col) == "Variation") %>%
    summarise(median = median(event_datetime)) %>%
    pull(median)
  
  # Date range
  control_range <- df_users %>%
    filter(!!sym(cohort_col) == "Control") %>%
    summarise(
      first = min(event_date),
      last = max(event_date),
      n_users = n()
    )
  
  variation_range <- df_users %>%
    filter(!!sym(cohort_col) == "Variation") %>%
    summarise(
      first = min(event_date),
      last = max(event_date),
      n_users = n()
    )
  
  # Return result
  tibble(
    method = method_name,
    metric = "event_datetime",
    ks_statistic = round(ks_test$statistic, 4),
    p_value = round(ks_test$p.value, 6),
    homogeneous = ks_test$p.value >= 0.05,
    control_median = as.character(control_median),
    variation_median = as.character(variation_median),
    control_first = as.character(control_range$first),
    control_last = as.character(control_range$last),
    control_n = control_range$n_users,
    variation_first = as.character(variation_range$first),
    variation_last = as.character(variation_range$last),
    variation_n = variation_range$n_users
  )
}
