library(tidyverse)
library(slider)
library(sf)

dengue_climate_joinville <- read_csv(here::here("data", "joinville", "dengue_climate_joinville.csv"))
dengue_climate_sc <- st_read(here::here("data", "joinville", "dengue_climate_sc.gpkg"))

# Calculate the 4-week 
dengue_climate_joinville <- dengue_climate_joinville %>%
  arrange(epiweek) %>%
  mutate(temp_avg_4w = slide_dbl(
    temp_med_avg,
    mean,
    .before = 4,   # look 4 steps back
    .after = -1,   # exclude current week
    .complete = TRUE  # returns NA if fewer than 4 observations
  ),
  umid_avg_4w = slide_dbl(
    umid_med_avg,
    mean,
    .before = 4,   # look 4 steps back
    .after = -1,   # exclude current week
    .complete = TRUE  # returns NA if fewer than 4 observations
  ),
  temp_min_avg_4w = slide_dbl(
    temp_min_avg,
    mean,
    .before = 4,   # look 4 steps back
    .after = -1,   # exclude current week
    .complete = TRUE  # returns NA if fewer than 4 observations
  ),
  temp_max_avg_4w = slide_dbl(
    temp_max_avg,
    mean,
    .before = 4,   # look 4 steps back
    .after = -1,   # exclude current week
    .complete = TRUE  # returns NA if fewer than 4 observations
  ),
  umid_min_avg_4w = slide_dbl(
    umid_min_avg,
    mean,
    .before = 4,   # look 4 steps back
    .after = -1,   # exclude current week
    .complete = TRUE  # returns NA if fewer than 4 observations
  ),
  umid_max_avg_4w = slide_dbl(
    umid_max_avg,
    mean,
    .before = 4,   # look 4 steps back
    .after = -1,   # exclude current week
    .complete = TRUE  # returns NA if fewer than 4 observations
  )
  )

dengue_climate_sc <- dengue_climate_sc %>%
  arrange(epiweek) %>%
  group_by(code_muni) %>%
  mutate(temp_avg_4w = slide_dbl(
    temp_med_avg,
    mean,
    .before = 4,   # look 4 steps back
    .after = -1,   # exclude current week
    .complete = TRUE  # returns NA if fewer than 4 observations
  ),
  umid_avg_4w = slide_dbl(
    umid_med_avg,
    mean,
    .before = 4,   # look 4 steps back
    .after = -1,   # exclude current week
    .complete = TRUE  # returns NA if fewer than 4 observations
  ),
  temp_min_avg_4w = slide_dbl(
    temp_min_avg,
    mean,
    .before = 4,   # look 4 steps back
    .after = -1,   # exclude current week
    .complete = TRUE  # returns NA if fewer than 4 observations
  ),
  temp_max_avg_4w = slide_dbl(
    temp_max_avg,
    mean,
    .before = 4,   # look 4 steps back
    .after = -1,   # exclude current week
    .complete = TRUE  # returns NA if fewer than 4 observations
  ),
  umid_min_avg_4w = slide_dbl(
    umid_min_avg,
    mean,
    .before = 4,   # look 4 steps back
    .after = -1,   # exclude current week
    .complete = TRUE  # returns NA if fewer than 4 observations
  ),
  umid_max_avg_4w = slide_dbl(
    umid_max_avg,
    mean,
    .before = 4,   # look 4 steps back
    .after = -1,   # exclude current week
    .complete = TRUE  # returns NA if fewer than 4 observations
  )
  ) %>%
  ungroup()

# calculate the 8-week
dengue_climate_joinville <- dengue_climate_joinville %>%
  arrange(epiweek) %>%
  mutate(temp_avg_8w = slide_dbl(
    temp_med_avg,
    mean,
    .before = 8,   # look 8 steps back
    .after = -1,   # exclude current week
    .complete = TRUE  # returns NA if fewer than 8 observations
  ),
  umid_avg_8w = slide_dbl(
    umid_med_avg,
    mean,
    .before = 8,   # look 8 steps back
    .after = -1,   # exclude current week
    .complete = TRUE  # returns NA if fewer than 8 observations
  ),
  temp_min_avg_8w = slide_dbl(
    temp_min_avg,
    mean,
    .before = 8,   # look 8 steps back
    .after = -1,   # exclude current week
    .complete = TRUE  # returns NA if fewer than 8 observations
  ),
  temp_max_avg_8w = slide_dbl(
    temp_max_avg,
    mean,
    .before = 8,   # look 8 steps back
    .after = -1,   # exclude current week
    .complete = TRUE  # returns NA if fewer than 8 observations
  ),
  umid_min_avg_8w = slide_dbl(
    umid_min_avg,
    mean,
    .before = 8,   # look 8 steps back
    .after = -1,   # exclude current week
    .complete = TRUE  # returns NA if fewer than 8 observations
  ),
  umid_max_avg_8w = slide_dbl(
    umid_max_avg,
    mean,
    .before = 8,   # look 8 steps back
    .after = -1,   # exclude current week
    .complete = TRUE  # returns NA if fewer than 8 observations
  )
  )

dengue_climate_sc <- dengue_climate_sc %>%
  arrange(epiweek) %>%
  group_by(code_muni) %>%
  mutate(temp_avg_8w = slide_dbl(
    temp_med_avg,
    mean,
    .before = 8,   # look 8 steps back
    .after = -1,   # exclude current week
    .complete = TRUE  # returns NA if fewer than 8 observations
  ),
  umid_avg_8w = slide_dbl(
    umid_med_avg,
    mean,
    .before = 8,   # look 8 steps back
    .after = -1,   # exclude current week
    .complete = TRUE  # returns NA if fewer than 8 observations
  ),
  temp_min_avg_8w = slide_dbl(
    temp_min_avg,
    mean,
    .before = 8,   # look 8 steps back
    .after = -1,   # exclude current week
    .complete = TRUE  # returns NA if fewer than 8 observations
  ),
  temp_max_avg_8w = slide_dbl(
    temp_max_avg,
    mean,
    .before = 8,   # look 8 steps back
    .after = -1,   # exclude current week
    .complete = TRUE  # returns NA if fewer than 8 observations
  ),
  umid_min_avg_8w = slide_dbl(
    umid_min_avg,
    mean,
    .before = 8,   # look 8 steps back
    .after = -1,   # exclude current week
    .complete = TRUE  # returns NA if fewer than 8 observations
  ),
  umid_max_avg_8w = slide_dbl(
    umid_max_avg,
    mean,
    .before = 8,   # look 8 steps back
    .after = -1,   # exclude current week
    .complete = TRUE  # returns NA if fewer than 8 observations
  )
) %>%
  ungroup()

# calculate the 12-week
dengue_climate_joinville <- dengue_climate_joinville %>%
  arrange(epiweek) %>%
  mutate(temp_avg_12w = slide_dbl(
    temp_med_avg,
    mean,
    .before = 12,   # look 12 steps back
    .after = -1,   # exclude current week
    .complete = TRUE  # returns NA if fewer than 12 observations
  ),
  umid_avg_12w = slide_dbl(
    umid_med_avg,
    mean,
    .before = 12,   # look 12 steps back
    .after = -1,   # exclude current week
    .complete = TRUE  # returns NA if fewer than 12 observations
  ),
  temp_min_avg_12w = slide_dbl(
    temp_min_avg,
    mean,
    .before = 12,   # look 12 steps back
    .after = -1,   # exclude current week
    .complete = TRUE  # returns NA if fewer than 12 observations
  ),
  temp_max_avg_12w = slide_dbl(
    temp_max_avg,
    mean,
    .before = 12,   # look 12 steps back
    .after = -1,   # exclude current week
    .complete = TRUE  # returns NA if fewer than 12 observations
  ),
  umid_min_avg_12w = slide_dbl(
    umid_min_avg,
    mean,
    .before = 12,   # look 12 steps back
    .after = -1,   # exclude current week
    .complete = TRUE  # returns NA if fewer than 12 observations
  ),
  umid_max_avg_12w = slide_dbl(
    umid_max_avg,
    mean,
    .before = 12,   # look 12 steps back
    .after = -1,   # exclude current week
    .complete = TRUE  # returns NA if fewer than 12 observations
  )
  )

dengue_climate_sc <- dengue_climate_sc %>%
  arrange(epiweek) %>%
  group_by(code_muni) %>%
  mutate(temp_avg_12w = slide_dbl(
    temp_med_avg,
    mean,
    .before = 12,   # look 12 steps back
    .after = -1,   # exclude current week
    .complete = TRUE  # returns NA if fewer than 12 observations
  ),
  umid_avg_12w = slide_dbl(
    umid_med_avg,
    mean,
    .before = 12,   # look 12 steps back
    .after = -1,   # exclude current week
    .complete = TRUE  # returns NA if fewer than 12 observations
  ),
  temp_min_avg_12w = slide_dbl(
    temp_min_avg,
    mean,
    .before = 12,   # look 12 steps back
    .after = -1,   # exclude current week
    .complete = TRUE  # returns NA if fewer than 12 observations
  ),
  temp_max_avg_12w = slide_dbl(
    temp_max_avg,
    mean,
    .before = 12,   # look 12 steps back
    .after = -1,   # exclude current week
    .complete = TRUE  # returns NA if fewer than 12 observations
  ),
  umid_min_avg_12w = slide_dbl(
    umid_min_avg,
    mean,
    .before = 12,   # look 12 steps back
    .after = -1,   # exclude current week
    .complete = TRUE  # returns NA if fewer than 12 observations
  ),
  umid_max_avg_12w = slide_dbl(
    umid_max_avg,
    mean,
    .before = 12,   # look 12 steps back
    .after = -1,   # exclude current week
    .complete = TRUE  # returns NA if fewer than 12 observations
  )
) %>%
  ungroup()

# calculate the 52-week rolling average 
dengue_climate_joinville <- dengue_climate_joinville %>%
  arrange(epiweek) %>%
  mutate(precip_avg_52w = slide_dbl(
    precip_total,
    mean,
    .before = 52,   # look 52 steps back
    .after = -1,   # exclude current week
    .complete = F
  )
)


# create trashold temp_med_avg
dengue_climate_joinville <- dengue_climate_joinville %>%
  arrange(epiweek) %>%
  mutate(temp_above_25 = pmax(temp_med_avg - 25, 0)
         )

# compute range of temp_med and umid_med in the last 4 weeks
dengue_climate_joinville <- dengue_climate_joinville %>%
  arrange(epiweek) %>%
  mutate(temp_med_range_4w = slide_dbl(
    temp_med_avg,
    ~ max(.x) - min(.x),
    .before = 4,   # look 4 steps back
    .after = -1,   # exclude current week
    .complete = TRUE  # returns NA if fewer than 4 observations
  ),
  umid_med_range_4w = slide_dbl(
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
dengue_climate_joinville <- dengue_climate_joinville[13:nrow(dengue_climate_joinville), ]

# remove the first 12 weeks of dengue_climate_sc
dengue_climate_sc <- dengue_climate_sc %>%
  group_by(code_muni) %>%
  arrange(epiweek) %>%
  slice(13:n()) %>%
  ungroup()

# standardize the covariates
dengue_climate_joinville <- dengue_climate_joinville %>%
  mutate(across(c(temp_avg_4w, umid_avg_4w, temp_avg_8w, umid_avg_8w, temp_avg_12w, umid_avg_12w,
                  temp_min_avg_4w, temp_max_avg_4w, umid_min_avg_4w, umid_max_avg_4w,
                  temp_min_avg_8w, temp_max_avg_8w, umid_min_avg_8w, umid_max_avg_8w,
                  temp_min_avg_12w, temp_max_avg_12w, umid_min_avg_12w, umid_max_avg_12w,
                  temp_med_lag1, temp_med_lag2, temp_med_lag3, temp_med_lag4,
                  umid_med_lag1, umid_med_lag2, umid_med_lag3, umid_med_lag4,
                  temp_med_range_4w, umid_med_range_4w, 
                  precip_avg_52w
                ),
                ~ (.x - mean(.x)) / sd(.x)
                )
         )
dengue_climate_sc <- dengue_climate_sc %>%
  group_by(code_muni) %>%
  mutate(across(c(temp_avg_4w, umid_avg_4w, temp_avg_8w, umid_avg_8w, temp_avg_12w, umid_avg_12w,
                  temp_min_avg_4w, temp_max_avg_4w, umid_min_avg_4w, umid_max_avg_4w,
                  temp_min_avg_8w, temp_max_avg_8w, umid_min_avg_8w, umid_max_avg_8w,
                  temp_min_avg_12w, temp_max_avg_12w, umid_min_avg_12w, umid_max_avg_12w),
                ~ (.x - mean(.x)) / sd(.x)
  )
  ) %>%
  ungroup()

# create ids for the INLA model
dengue_climate_joinville <- dengue_climate_joinville %>%
  mutate(year = as.numeric(substr(epiweek, 1, 4)))
dengue_climate_joinville$week_id <- as.numeric(substr(dengue_climate_joinville$epiweek, 5, 6))
dengue_climate_joinville$time_id <- as.numeric(factor(dengue_climate_joinville$epiweek))
dengue_climate_joinville$year_id <- as.numeric(factor(dengue_climate_joinville$year))
dengue_climate_joinville$obs_id <- 1:nrow(dengue_climate_joinville)
row.names(dengue_climate_joinville) <- dengue_climate_joinville$obs_id
write_csv(dengue_climate_joinville, here::here("data", "joinville", "dengue_climate_joinville_inla.csv"))

dengue_climate_sc <- dengue_climate_sc %>%
  mutate(year = as.numeric(substr(epiweek, 1, 4)))
dengue_climate_sc$week_id <- as.numeric(substr(dengue_climate_sc$epiweek, 5, 6))
dengue_climate_sc$time_id <- as.numeric(factor(dengue_climate_sc$epiweek))
dengue_climate_sc$year_id <- as.numeric(factor(dengue_climate_sc$year))
dengue_climate_sc$obs_id <- 1:nrow(dengue_climate_sc)
row.names(dengue_climate_sc) <- dengue_climate_sc$obs_id

# create id for the city
dengue_climate_sc <- dengue_climate_sc %>%
  group_by(code_muni) %>%
  mutate(city_id = cur_group_id()) %>%
  ungroup()

st_write(dengue_climate_sc, here::here("data", "joinville", "dengue_climate_sc_inla.gpkg"), delete_layer = TRUE)
