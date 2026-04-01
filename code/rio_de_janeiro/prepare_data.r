library(tidyverse)
library(slider)

dengue_climate_rj <- read_csv("data/rio_de_janeiro/dengue_climate_rj.csv")

dengue_climate_rj <- dengue_climate_rj %>%
  arrange(epiweek) %>%
  mutate(temp_avg_4w = slide_dbl(
    temp_med_avg,
    mean,
    .before = 4,   # look 4 steps back
    .after = -1,   # exclude current week
    .complete = F  # returns NA if fewer than 4 observations
  ),
  umid_avg_4w = slide_dbl(
    umid_med_avg,
    mean,
    .before = 4,   # look 4 steps back
    .after = -1,   # exclude current week
    .complete = F  # returns NA if fewer than 4 observations
  ),
  temp_min_avg_4w = slide_dbl(
    temp_min_avg,
    mean,
    .before = 4,   # look 4 steps back
    .after = -1,   # exclude current week
    .complete = F  # returns NA if fewer than 4 observations
  ),
  temp_max_avg_4w = slide_dbl(
    temp_max_avg,
    mean,
    .before = 4,   # look 4 steps back
    .after = -1,   # exclude current week
    .complete = F  # returns NA if fewer than 4 observations
  ),
  umid_min_avg_4w = slide_dbl(
    umid_min_avg,
    mean,
    .before = 4,   # look 4 steps back
    .after = -1,   # exclude current week
    .complete = F  # returns NA if fewer than 4 observations
  ),
  umid_max_avg_4w = slide_dbl(
    umid_max_avg,
    mean,
    .before = 4,   # look 4 steps back
    .after = -1,   # exclude current week
    .complete = F  # returns NA if fewer than 4 observations
  )
  )

dengue_climate_rj <- dengue_climate_rj %>%
  arrange(epiweek) %>%
  mutate(temp_avg_8w = slide_dbl(
    temp_med_avg,
    mean,
    .before = 8,   # look 8 steps back
    .after = -1,   # exclude current week
    .complete = F  # returns NA if fewer than 8 observations
  ),
  umid_avg_8w = slide_dbl(
    umid_med_avg,
    mean,
    .before = 8,   # look 8 steps back
    .after = -1,   # exclude current week
    .complete = F  # returns NA if fewer than 8 observations
  ),
  temp_min_avg_8w = slide_dbl(
    temp_min_avg,
    mean,
    .before = 8,   # look 8 steps back
    .after = -1,   # exclude current week
    .complete = F  # returns NA if fewer than 8 observations
  ),
  temp_max_avg_8w = slide_dbl(
    temp_max_avg,
    mean,
    .before = 8,   # look 8 steps back
    .after = -1,   # exclude current week
    .complete = F  # returns NA if fewer than 8 observations
  ),
  umid_min_avg_8w = slide_dbl(
    umid_min_avg,
    mean,
    .before = 8,   # look 8 steps back
    .after = -1,   # exclude current week
    .complete = F  # returns NA if fewer than 8 observations
  ),
  umid_max_avg_8w = slide_dbl(
    umid_max_avg,
    mean,
    .before = 8,   # look 8 steps back
    .after = -1,   # exclude current week
    .complete = F  # returns NA if fewer than 8 observations
  )
  )

dengue_climate_rj <- dengue_climate_rj %>%
  arrange(epiweek) %>%
  mutate(temp_avg_12w = slide_dbl(
    temp_med_avg,
    mean,
    .before = 12,   # look 12 steps back
    .after = -1,   # exclude current week
    .complete = F  # returns NA if fewer than 12 observations
  ),
  umid_avg_12w = slide_dbl(
    umid_med_avg,
    mean,
    .before = 12,   # look 12 steps back
    .after = -1,   # exclude current week
    .complete = F  # returns NA if fewer than 12 observations
  ),
  temp_min_avg_12w = slide_dbl(
    temp_min_avg,
    mean,
    .before = 12,   # look 12 steps back
    .after = -1,   # exclude current week
    .complete = F  # returns NA if fewer than 12 observations
  ),
  temp_max_avg_12w = slide_dbl(
    temp_max_avg,
    mean,
    .before = 12,   # look 12 steps back
    .after = -1,   # exclude current week
    .complete = F  # returns NA if fewer than 12 observations
  ),
  umid_min_avg_12w = slide_dbl(
    umid_min_avg,
    mean,
    .before = 12,   # look 12 steps back
    .after = -1,   # exclude current week
    .complete = F  # returns NA if fewer than 12 observations
  ),
  umid_max_avg_12w = slide_dbl(
    umid_max_avg,
    mean,
    .before = 12,   # look 12 steps back
    .after = -1,   # exclude current week
    .complete = F  # returns NA if fewer than 12 observations
  )
  )

dengue_climate_rj <- dengue_climate_rj %>%
  arrange(epiweek) %>%
  mutate(precip_avg_52w = slide_dbl(
    precip_total,
    mean,
    .before = 52,   # look 52 steps back
    .after = -1,   # exclude current week
    .complete = F
  )
)

# standardize the covariates
dengue_climate_rj <- dengue_climate_rj %>%
  mutate(across(c(temp_avg_4w, umid_avg_4w, temp_min_avg_4w, temp_max_avg_4w, umid_min_avg_4w, umid_max_avg_4w,
                  temp_avg_8w, umid_avg_8w, temp_min_avg_8w, temp_max_avg_8w, umid_min_avg_8w, umid_max_avg_8w,
                  temp_avg_12w, umid_avg_12w, temp_min_avg_12w, temp_max_avg_12w, umid_min_avg_12w, umid_max_avg_12w,
                  precip_avg_52w),
                ~ (. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE)))

# remove rows with NA values (due to the moving averages)
dengue_climate_rj <- dengue_climate_rj[2:nrow(dengue_climate_rj), ]

# create ids for the INLA model
dengue_climate_rj <- dengue_climate_rj %>%
  mutate(year = as.numeric(substr(epiweek, 1, 4)))
dengue_climate_rj$week_id <- as.numeric(substr(dengue_climate_rj$epiweek, 5, 6))
dengue_climate_rj$time_id <- as.numeric(factor(dengue_climate_rj$epiweek))
dengue_climate_rj$year_id <- as.numeric(factor(dengue_climate_rj$year))
dengue_climate_rj$obs_id <- 1:nrow(dengue_climate_rj)
row.names(dengue_climate_rj) <- dengue_climate_rj$obs_id
write_csv(dengue_climate_rj, here::here("data", "rio_de_janeiro", "dengue_climate_rj_inla.csv"))
