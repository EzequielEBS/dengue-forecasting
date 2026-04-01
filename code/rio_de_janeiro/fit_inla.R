library(INLA)
library(tidyverse)
library(ggplot2)
library(purrr)
library(sf)
library(geobr)
library(spdep)
library(pbapply)

# https://www.paulamoraga.com/book-geospatial/sec-inla.html
# https://github.com/chlobular/ghr-imdc-2025
# https://github.com/anabento/DengueSprint_Cornell-PEH
# https://github.com/marciomacielbastos/MosqlimateSprint2025
# https://github.com/lsbastos/sprint2025

source("code/aux_func.r")
inla.setOption(num.threads = "15:1")

# Load the data
dengue_climate_rj <- read_csv("data/rio_de_janeiro/dengue_climate_rj_inla.csv")
# dengue_climate_sc <- st_read("data/rio_de_janeiro/dengue_climate_sc_inla.gpkg")
# dengue_climate_sc <- dengue_climate_sc %>%
#   select(data_iniSE, 
#           epiweek, 
#           time_id,
#           year_id, 
#           week_id, 
#           city_id, 
#           obs_id,
#           code_muni,
#           casos, 
#           pop, 
#           temp_avg_8w, 
#           temp_min_avg_8w, 
#           temp_max_avg_8w, 
#           temp_avg_12w, 
#           temp_min_avg_12w, 
#           temp_max_avg_12w,
#         ) %>%
#   st_drop_geometry()

quantiles <- c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975)
start_week <- 525
len_windows <- 8

# Model 0
f0 <- casos ~ 1 + 
    f(time_id, model = "rw1") +
    f(week_id, model = "rw1", cyclic = TRUE, constr = TRUE,
      group = year_id, control.group = list(model = "ar1"))
family0 <- "poisson"
results_M0 <- transpose(pblapply(start_week:(nrow(dengue_climate_rj) - len_windows),
function(i) {
  fit <- run_inla_model(dengue_climate_rj, 
                        outcome = "casos",
                        threshold_week = i,
                        formula = f0, 
                        family = family0,
                        quantiles = quantiles,
                        len_windows = len_windows
                      )
  return(fit)
}))
saveRDS(results_M0, file = "results/rio_de_janeiro/results_M0.rds")

# Model 1 
f1 <- casos ~ 1 + 
    temp_avg_8w + 
    f(time_id, model = "rw1") +
    f(week_id, model = "rw1", cyclic = TRUE, constr = TRUE,
      group = year_id, control.group = list(model = "ar1"))   
family1 <- "poisson"

results_M1 <- transpose(pblapply(start_week:(nrow(dengue_climate_rj) - len_windows),
function(i) {
  fit <- run_inla_model(dengue_climate_rj, 
                        outcome = "casos",
                        threshold_week = i,
                        formula = f1, 
                        family = family1,
                        quantiles = quantiles,
                        len_windows = len_windows
                      )
  return(fit)
}))
saveRDS(results_M1, file = "results/rio_de_janeiro/results_M1.rds")

# Model 2
f2 <- casos ~ 1 + 
    temp_min_avg_8w + 
    f(time_id, model = "rw1") +
    f(week_id, model = "rw1", cyclic = TRUE, constr = TRUE,
      group = year_id, control.group = list(model = "ar1"))
family2 <- "poisson"
results_M2 <- transpose(pblapply(start_week:(nrow(dengue_climate_rj) - len_windows),
function(i) {
  fit <- run_inla_model(dengue_climate_rj,
                        outcome = "casos", 
                        threshold_week = i,
                        formula = f2, 
                        family = family2,
                        quantiles = quantiles,
                        len_windows = len_windows
                      )
  return(fit)
}))
saveRDS(results_M2, file = "results/rio_de_janeiro/results_M2.rds")

# Model 3
f3 <- casos ~ 1 + 
    temp_max_avg_8w +
    f(time_id, model = "rw1") +
    f(week_id, model = "rw1", cyclic = TRUE, constr = TRUE,
      group = year_id, control.group = list(model = "ar1"))
family3 <- "poisson"
results_M3 <- transpose(pblapply(start_week:(nrow(dengue_climate_rj) - len_windows),
function(i) {
  fit <- run_inla_model(dengue_climate_rj,
                        outcome = "casos", 
                        threshold_week = i,
                        formula = f3, 
                        family = family3,
                        quantiles = quantiles,
                        len_windows = len_windows
                      )
  return(fit)
}))
saveRDS(results_M3, file = "results/rio_de_janeiro/results_M3.rds")

# Model 4
f4 <- casos ~ 1 + 
    temp_avg_12w +
    f(time_id, model = "rw1") +
    f(week_id, model = "rw1", constr = TRUE, cyclic = TRUE,
      group = year_id, control.group = list(model = "ar1"))
family4 <- "poisson"
results_M4 <- transpose(pblapply(start_week:(nrow(dengue_climate_rj) - len_windows),
function(i) {
  fit <- run_inla_model(dengue_climate_rj, 
                        outcome = "casos",
                        threshold_week = i,
                        formula = f4, 
                        family = family4,
                        quantiles = quantiles,
                        len_windows = len_windows
                      )
  return(fit)
}))
saveRDS(results_M4, file = "results/rio_de_janeiro/results_M4.rds")

# Model 5
f5 <- casos ~ 1 + 
    temp_min_avg_12w +
    f(time_id, model = "rw1") +
    f(week_id, model = "rw1", cyclic = TRUE, constr = TRUE,
      group = year_id, control.group = list(model = "ar1"))
family5 <- "poisson"
results_M5 <- transpose(pblapply(start_week:(nrow(dengue_climate_rj) - len_windows),
function(i) {
  fit <- run_inla_model(dengue_climate_rj, 
                        outcome = "casos",
                        threshold_week = i, 
                        formula = f5, 
                        family = family5,
                        quantiles = quantiles,
                        len_windows = len_windows
                      )
  return(fit)
}))

saveRDS(results_M5, file = "results/rio_de_janeiro/results_M5.rds")

# Model 6
f6 <- casos ~ 1 + 
    temp_max_avg_12w +
    f(time_id, model = "rw1") +
    f(week_id, model = "rw1", cyclic = TRUE, constr = TRUE,
      group = year_id, control.group = list(model = "ar1"))
family6 <- "poisson"
results_M6 <- transpose(pblapply(start_week:(nrow(dengue_climate_rj) - len_windows), 
function(i) {
  fit <- run_inla_model(dengue_climate_rj, 
                        outcome = "casos",
                        threshold_week = i,
                        formula = f6, 
                        family = family6,
                        quantiles = quantiles,
                        len_windows = len_windows
                      )
  return(fit)
}))

saveRDS(results_M6, file = "results/rio_de_janeiro/results_M6.rds")

f7 <- casos ~ 1 +
    f(week_id, model = "rw2", constr = T, cyclic = T,
      hyper = list(
        # Precision of unstructure random effects
        prec = list(
          prior="pc.prec",
          param=c(3, 0.01)
        )
      )
    ) + 
    f(year_id, model = "iid", constr = T,
      hyper = list(
        # Precision of unstructure random effects
        prec = list(
          prior="pc.prec",
          param=c(3, 0.01)
        )
      )
    )
family7 <- "nbinomial"
results_M7 <- transpose(pblapply(start_week:(nrow(dengue_climate_rj) - len_windows), 
function(i) {
  fit <- run_inla_model(dengue_climate_rj, 
                        outcome = "casos",
                        threshold_week = i,
                        formula = f7, 
                        family = family7,
                        quantiles = quantiles,
                        len_windows = len_windows
                      )
  return(fit)
}))
saveRDS(results_M7, file = "results/rio_de_janeiro/results_M7.rds")
