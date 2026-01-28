# ---- libraries ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here)

### 01. Load raw data, transform, and save in parquet format ----
source(here("03.scripts", "01_load_data.R"))

### 02. Data quality assessment and cleaning steps ----
source(here("03.scripts", "02_clean_data.R"))

### 03. Compile data quality summary outputs ----
source(here("03.scripts", "03_data_quality_summary.R"))

### 04. Diagnose user contamination (users assigned to both cohorts) ----
source(here("03.scripts", "04_diagnose_users_contamination.R"))

### 04b. Language stratification: Diagnose contamination for English users ----
source(here("03.scripts", "04_EN_diagnose_users_contamination.R"))

### 05. Evaluate metrics by test phases (50/50 vs 90/10 allocation periods) ----
source(here("03.scripts", "05_evaluate_by_phases.R"))

### 06. Evaluate metrics by app release phases ----
source(here("03.scripts", "06_app_release.R"))

### 07. Homogeneity tests (ABORTED: multi-variant allocation prevents reliable testing) ----
source(here("03.scripts", "07_homogeneity_tests.R"))
