library(tidyverse)
library(slider)

dengue_climate_joinville <- read_csv(here::here("data", "joinville", "dengue_climate_joinville.csv"))

# Calculate the 4-week average for temp_med, umid_med, and precip_total
dengue_climate_joinville <- dengue_climate_joinville %>%
  arrange(epiweek) %>%
  mutate(temp_avg_4w = slide_dbl(
    temp_med_avg,
    mean,
    .before = 4,   # look 4 steps back
    .after = -1,   # exclude current week
    .complete = TRUE  # returns NA if fewer than 4 observations
  ))

dengue_climate_joinville <- dengue_climate_joinville %>%
  arrange(epiweek) %>%
  mutate(umid_avg_4w = slide_dbl(
    umid_med_avg,
    mean,
    .before = 4,   # look 4 steps back
    .after = -1,   # exclude current week
    .complete = TRUE  # returns NA if fewer than 4 observations
  ))

dengue_climate_joinville <- dengue_climate_joinville %>%
  arrange(epiweek) %>%
  mutate(precip_avg_4w = slide_dbl(
    precip_total,
    mean,
    .before = 4,   # look 4 steps back
    .after = -1,   # exclude current week
    .complete = TRUE  # returns NA if fewer than 4 observations
  ))

# create trashold temp_med_avg
dengue_climate_joinville <- dengue_climate_joinville %>%
  arrange(epiweek) %>%
  mutate(temp_above_25 = pmax(temp_med_avg - 25, 0),
         temp_below_25 = pmin(temp_med_avg, 25)
         )

# compute variability of temp_med and umid_med in the last 4 weeks
dengue_climate_joinville <- dengue_climate_joinville %>%
  arrange(epiweek) %>%
  mutate(temp_med_sd_4w = slide_dbl(
    temp_med_avg,
    sd,
    .before = 4,   # look 4 steps back
    .after = -1,   # exclude current week
    .complete = TRUE  # returns NA if fewer than 4 observations
  ))
dengue_climate_joinville <- dengue_climate_joinville %>%
  arrange(epiweek) %>%
  mutate(umid_med_sd_4w = slide_dbl(
    umid_med_avg,
    sd,
    .before = 4,   # look 4 steps back
    .after = -1,   # exclude current week
    .complete = TRUE  # returns NA if fewer than 4 observations
  ))

# compute range of temp_med and umid_med in the last 4 weeks
dengue_climate_joinville <- dengue_climate_joinville %>%
  arrange(epiweek) %>%
  mutate(temp_med_range_4w = slide_dbl(
    temp_med_avg,
    ~ max(.x) - min(.x),
    .before = 4,   # look 4 steps back
    .after = -1,   # exclude current week
    .complete = TRUE  # returns NA if fewer than 4 observations
  ))
dengue_climate_joinville <- dengue_climate_joinville %>%
  arrange(epiweek) %>%
  mutate(umid_med_range_4w = slide_dbl(
    umid_med_avg,
    ~ max(.x) - min(.x),
    .before = 4,   # look 4 steps back
    .after = -1,   # exclude current week
    .complete = TRUE  # returns NA if fewer than 4 observations
  ))

# create lags for temp_med and umid_med
dengue_climate_joinville <- dengue_climate_joinville %>%
  arrange(epiweek) %>%
  mutate(temp_med_lag1 = lag(temp_med_avg, 1),
          temp_med_lag2 = lag(temp_med_avg, 2),
          temp_med_lag3 = lag(temp_med_avg, 3),
          temp_med_lag4 = lag(temp_med_avg, 4),
          umid_med_lag1 = lag(umid_med_avg, 1),
          umid_med_lag2 = lag(umid_med_avg, 2),
          umid_med_lag3 = lag(umid_med_avg, 3),
          umid_med_lag4 = lag(umid_med_avg, 4)
        )


# remove rows with NA values (due to lag and rolling average)
dengue_climate_joinville <- dengue_climate_joinville %>%
  filter(!is.na(temp_avg_4w) & 
          !is.na(umid_avg_4w) & 
          !is.na(precip_avg_4w) &
          !is.na(temp_med_lag1) &
          !is.na(temp_med_lag2) &
          !is.na(temp_med_lag3) &
          !is.na(temp_med_lag4) &
          !is.na(umid_med_lag1) &
          !is.na(umid_med_lag2) &
          !is.na(umid_med_lag3) &
          !is.na(umid_med_lag4)
         )


# create ids for the INLA model
dengue_climate_joinville <- dengue_climate_joinville %>%
  mutate(year = as.numeric(substr(epiweek, 1, 4)))
dengue_climate_joinville$week_id <- as.numeric(substr(dengue_climate_joinville$epiweek, 5, 6))
dengue_climate_joinville$time_id <- as.numeric(factor(dengue_climate_joinville$epiweek))
dengue_climate_joinville$year_id <- as.numeric(factor(dengue_climate_joinville$year))
dengue_climate_joinville$obs_id <- 1:nrow(dengue_climate_joinville)
row.names(dengue_climate_joinville) <- dengue_climate_joinville$obs_id
write_csv(dengue_climate_joinville, here::here("data", "joinville", "dengue_climate_joinville_inla.csv"))
