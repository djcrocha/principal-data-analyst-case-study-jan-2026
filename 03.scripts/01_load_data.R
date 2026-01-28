# ---- libraries ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readxl, here, tidyverse, janitor, arrow)

# ---- vars ----
path_input  <- here("02.data", "raw", "ab-test-dataset.xlsx")
path_output <- here("02.data", "prep", "raw_ab-test-dataset.parquet")
sheetname <- "Banner AF_2026-01-20-1806"

# ---- load data ----
if (!file.exists(path_input)) {stop(paste("file not found: ", path_input))}

## ---- check sheets names ----
sheets <- 
  excel_sheets(path_input)

## ---- load excel file ----
df_raw <-
  read_excel(path_input,sheet = sheetname)  %>%
  mutate(across(where(is.character), as.factor)) %>%
  clean_names()

# --- write rds ----
dir.create(dirname(path_output), showWarnings = FALSE, recursive = TRUE)
write_parquet(df_raw, path_output)

# ---- free memory ---- 
rm(list = setdiff(ls(), c()))
gc(full = TRUE)
