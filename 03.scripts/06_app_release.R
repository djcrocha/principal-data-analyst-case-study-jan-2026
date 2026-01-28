#Hypothesis: App release on June 15 caused allocation shift from 50/50 to 90/10

# ---- libraries & setup ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, tidyverse, arrow, scales, purrr)
if (!exists("load_global_utils")) {source(here("03.scripts", "global_utils.R"))}

# ---- load data ----
path_input <- here("02.data", "prep", "raw_ab-test-dataset.parquet")
df_cleaned <- read_parquet(path_input)

app_coverage <-
  df_cleaned %>%
  filter(!grepl("staging", release_version)) %>%
  group_by(event_date, release_version) %>%
  summarise(qty_app_install = n_distinct(user_id)) %>%
  group_by(event_date) %>%
  mutate(share_versions = qty_app_install / sum(qty_app_install))

app_coverage %>%
  ggplot()+
  geom_line(aes(event_date,share_versions, 
                colour = release_version, group = release_version))


# Allocation ratio remained consistent at 90/10 from June 15 until experiment end. 
# However, multiple app version releases occurred during this period, with none 
# achieving the same sustained coverage as the allocation pattern—suggesting the 
# allocation shift was platform-driven, not app-related.
  