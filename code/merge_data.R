library(tidyverse)

# Load the data
dengue_rj <- read_csv("data/rio_de_janeiro/dengue_rj.csv")
dengue_joinville <- read_csv("data/joinville/dengue_joinville.csv")
climate_rj_weekly <- read_csv("data/rio_de_janeiro/climate_rj_weekly.csv")
climate_joinville_weekly <- read_csv("data/joinville/climate_joinville_weekly.csv")

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
  write_csv("data/rio_de_janeiro/dengue_climate_rj.csv")
dengue_climate_joinville <- dengue_joinville %>%
  rename(epiweek = "SE") %>%
  left_join(climate_joinville_weekly, by = "epiweek") %>%
  # select(-c(geocodigo,
  #           temp_min_avg,
  #           temp_max_avg,
  #           temp_med_avg,
  #           umid_max_avg,
  #           umid_med_avg,
  #           umid_min_avg
  # )) %>%
  write_csv("data/joinville/dengue_climate_joinville.csv")
