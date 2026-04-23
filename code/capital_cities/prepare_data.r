library(tidyverse)
library(slider)

states <- list(
  "AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA",
  "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", 
  "RO", "RR", "RS", "SC", "SE", "SP", "TO"
)

data_capital_cities <- lapply(states, function(uf) {
  data <- read_csv(paste0("data/capital_cities/dengue_climate_", uf, ".csv"))
  data <- data %>%
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

  data <- data %>%
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

  data <- data %>%
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

  data <- data %>%
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
  data <- data %>%
    mutate(across(c(temp_avg_4w, umid_avg_4w, temp_min_avg_4w, temp_max_avg_4w, umid_min_avg_4w, umid_max_avg_4w,
                    temp_avg_8w, umid_avg_8w, temp_min_avg_8w, temp_max_avg_8w, umid_min_avg_8w, umid_max_avg_8w,
                    temp_avg_12w, umid_avg_12w, temp_min_avg_12w, temp_max_avg_12w, umid_min_avg_12w, umid_max_avg_12w,
                    precip_avg_52w),
                  ~ (. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE)))

  # remove rows with NA values (due to the moving averages)
  data <- data[2:nrow(data), ]

  # create ids for the INLA model
  data <- data %>%
    mutate(year = as.numeric(substr(epiweek, 1, 4)))
  data$week_id <- as.numeric(substr(data$epiweek, 5, 6))
  data$time_id <- as.numeric(factor(data$epiweek))
  data$year_id <- as.numeric(factor(data$year))
  data$obs_id <- 1:nrow(data)
  row.names(data) <- data$obs_id
  write_csv(data, here::here("data", "capital_cities", 
                          paste0("dengue_climate_", uf, "_inla.csv")))
  return(data)
})



