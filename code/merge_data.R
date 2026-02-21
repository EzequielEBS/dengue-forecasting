library(tidyverse)

# Load the data
dengue_rj <- read_csv("data/dengue_rj_from2025.csv")
climate_rj_weekly <- read_csv("data/climate_rj_from2025_weekly.csv")

# Merge the datasets by epiweek 
dengue_climate_rj <- dengue_rj %>%
  rename(epiweek = "SE") %>%
  left_join(climate_rj_weekly, by = "epiweek") %>%
  select(-c(geocodigo,
            temp_min_avg,
            temp_max_avg,
            temp_med_avg,
            umid_max_avg,
            umid_med_avg,
            umid_min_avg
            )) %>%
  write_csv("data/dengue_climate_rj_from2025.csv")
