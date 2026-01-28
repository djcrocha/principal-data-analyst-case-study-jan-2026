# Hostelworld A/B Test Analysis
**Principal Data Analyst Case Study - January 2026**  
**Presented by: Daniel Rocha**

---

## Executive Summary

Analysis of a 30-day A/B test evaluating a new search feature on Hostelworld's mobile app. 
---

## Reproducing the Analysis

### Prepare the Raw Dataset
Place the original Excel file provided by Hostelworld in the raw data directory:

01.data/raw/ab-test-dataset.xlsx

**Note**: The `01_load_data.R` script expects the file in this exact location.

### Prerequisites
- R version 4.x or higher
- RStudio (recommended)

### Setup

```r
# 1. Install renv if needed
install.packages("renv")

# 2. Restore project dependencies
renv::restore()

# 3. Run analysis pipeline
source("main.R")
```

### Key Dependencies
- `dplyr`, `tidyr` - Data manipulation
- `ggplot2` - Visualization
- `arrow` - Parquet file handling
- `here` - Path management

Full dependency list available in `renv.lock`

