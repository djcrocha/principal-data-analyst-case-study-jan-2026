# ---- libraries & utils ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here)

if (!exists("load_global_utils")) {source(here("03.scripts", "global_utils.R"))}

# ---- Data Quality Summary ----

# Load raw dataset for comparison
df_raw_original <- 
  read_parquet(here("02.data", "prep", "raw_ab-test-dataset.parquet"))

# Load clean dataset for comparison
df_cleaned <- 
  read_parquet(here("02.data", "prep", "CLEANED_ab-test-dataset.parquet"))

# Load cleaning log
log_df <- 
  read_tsv(here("02.data", "prep", "data_clean_log.tsv"), show_col_types = FALSE)

# Get all columns from raw data
raw_columns <- colnames(df_raw_original)

# Get columns that were checked in log
checked_columns <- unique(log_df$Column)

# Identify unchecked columns
unchecked_columns <- setdiff(raw_columns, checked_columns)

# Summary statistics
summary_stats <- tibble(
  metric = c(
    "Raw dataset rows",
    "Cleaned dataset rows",
    "Rows removed",
    "Rows removed (%)"
  ),
  value = c(
    nrow(df_raw_original),
    nrow(df_cleaned),
    nrow(df_raw_original) - nrow(df_cleaned),
    sprintf("%.1f%%", (nrow(df_raw_original) - nrow(df_cleaned)) / nrow(df_raw_original) * 100)
  )
)
