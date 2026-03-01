library(INLA)
library(tidyverse)
library(ggplot2)
library(purrr)

source("code/aux_func.r")

# Load the data
dengue_climate_joinville <- read_csv("data/joinville/dengue_climate_joinville_inla.csv")

# Model 1 
f1 <- casos ~ 1 + 
    temp_avg_4w + umid_avg_4w +
    f(time_id, model = "rw1") +
    f(week_id, model = "rw1", cyclic = TRUE, constr = TRUE,
      group = year_id, control.group = list(model = "ar1"))   
family1 <- "poisson"

quantiles <- c(0.05, 0.1, 0.2, 0.5, 0.8, 0.9, 0.95)
results_M1 <- transpose(lapply(261:(nrow(dengue_climate_joinville) - 3), function(i) {
  fit <- run_inla_model(dengue_climate_joinville, 
                        trashold_week = i, 
                        formula = f1, 
                        family = family1,
                        quantiles = quantiles
                      )
  return(fit)
}))
saveRDS(results_M1, file = "results/joinville/results_M1.rds")

# Model 2
f2 <- casos ~ 1 + 
    temp_above_25 + 
    f(time_id, model = "rw1") +
    f(week_id, model = "rw1", cyclic = TRUE, constr = TRUE,
      group = year_id, control.group = list(model = "ar1"))
family2 <- "poisson"
results_M2 <- transpose(lapply(261:(nrow(dengue_climate_joinville) - 3), function(i) {
  fit <- run_inla_model(dengue_climate_joinville, 
                        trashold_week = i, 
                        formula = f2, 
                        family = family2,
                        quantiles = quantiles
                      )
  return(fit)
}))
saveRDS(results_M2, file = "results/joinville/results_M2.rds")

# Model 3
f3 <- casos ~ 1 + 
    temp_med_sd_4w + umid_med_sd_4w +
    f(time_id, model = "rw1") +
    f(week_id, model = "rw1", cyclic = TRUE, constr = TRUE,
      group = year_id, control.group = list(model = "ar1"))
family3 <- "poisson"
results_M3 <- transpose(lapply(261:(nrow(dengue_climate_joinville) - 3), function(i) {
  fit <- run_inla_model(dengue_climate_joinville, 
                        trashold_week = i, 
                        formula = f3, 
                        family = family3,
                        quantiles = quantiles
                      )
  return(fit)
}))
saveRDS(results_M3, file = "results/joinville/results_M3.rds")

# Model 4
f4 <- casos ~ 1 + 
    temp_med_range_4w +
    f(time_id, model = "rw1") +
    f(week_id, model = "rw1", cyclic = TRUE, constr = TRUE,
      group = year_id, control.group = list(model = "ar1"))
family4 <- "poisson"
results_M4 <- transpose(lapply(261:(nrow(dengue_climate_joinville) - 3), function(i) {
  fit <- run_inla_model(dengue_climate_joinville, 
                        trashold_week = i, 
                        formula = f4, 
                        family = family4,
                        quantiles = quantiles
                      )
  return(fit)
}))
saveRDS(results_M4, file = "results/joinville/results_M4.rds")
