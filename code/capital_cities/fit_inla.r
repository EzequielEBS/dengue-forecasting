library(INLA)
library(tidyverse)
library(ggplot2)
library(purrr)
library(pbapply)
library(parallel)
library(qs2)

# https://www.paulamoraga.com/book-geospatial/sec-inla.html
# https://github.com/chlobular/ghr-imdc-2025
# https://github.com/anabento/DengueSprint_Cornell-PEH
# https://github.com/marciomacielbastos/MosqlimateSprint2025
# https://github.com/lsbastos/sprint2025

source("code/aux_func.r")
# inla.setOption(num.threads = "15:1")

states <- list(
  "AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA",
  "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", 
  "RO", "RR", "RS", "SC", "SE", "SP", "TO"
)
concluded <- list(
  "AC",
  "AL", 
  "AM", 
  "AP", 
  "BA", 
  "CE", 
  "DF", 
  "ES", 
  "GO", 
  "MA",
  "MG",
  "MS", 
  "MT", 
  "PA", 
  "PB", 
  "PE", 
  "PI", 
  "PR", 
  "RJ", 
  "RN",
  "RO", 
  "RR", 
  # "RS", 
  "SC",
  "SE",
  "SP",
  "TO"
)

quantiles <- c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975)
len_windows <- 8



results <- lapply(states, function(uf) {
  if (uf %in% concluded) {
    print(paste0("Results for ", uf, " already exist. Skipping..."))
    return(NULL)
  }
  dengue_climate <- read_csv(paste0("data/capital_cities/dengue_climate_", uf, "_inla.csv"))
  start_week <- dengue_climate[dengue_climate$epiweek == 202452, "time_id"]$time_id

  # Model 0
  f0 <- casos ~ 1 + 
      f(time_id, model = "rw1") +
      f(week_id, model = "rw1", cyclic = TRUE, constr = TRUE,
        group = year_id, control.group = list(model = "ar1")) +
      f(year_id, model = "iid", constr = T)
  family0 <- "poisson"
  print(paste0("Fitting model 0 for ", uf))
  results_M0 <- transpose(pblapply(start_week:(nrow(dengue_climate) - len_windows),
  function(i) {
    fit <- run_inla_model(dengue_climate, 
                          outcome = "casos",
                          threshold_week = i,
                          formula = f0, 
                          family = family0,
                          quantiles = quantiles,
                          len_windows = len_windows
                        )
    return(fit)
  }
  ))
  # Model 1 
  f1 <- casos ~ 1 + 
      temp_avg_8w + 
      f(time_id, model = "rw1") +
      f(week_id, model = "rw1", cyclic = TRUE, constr = TRUE,
        group = year_id, control.group = list(model = "ar1")) +
      f(year_id, model = "iid", constr = T)
  family1 <- "poisson"
  print(paste0("Fitting model 1 for ", uf))
  results_M1 <- transpose(pblapply(start_week:(nrow(dengue_climate) - len_windows),
  function(i) {
    fit <- run_inla_model(dengue_climate, 
                          outcome = "casos",
                          threshold_week = i,
                          formula = f1, 
                          family = family1,
                          quantiles = quantiles,
                          len_windows = len_windows
                        )
    return(fit)
  }))
  # Model 2
  f2 <- casos ~ 1 + 
      temp_min_avg_8w + 
      f(time_id, model = "rw1") +
      f(week_id, model = "rw1", cyclic = TRUE, constr = TRUE,
        group = year_id, control.group = list(model = "ar1")) +
      f(year_id, model = "iid", constr = T)
  family2 <- "poisson"
  print(paste0("Fitting model 2 for ", uf))
  results_M2 <- transpose(pblapply(start_week:(nrow(dengue_climate) - len_windows),
  function(i) {
    fit <- run_inla_model(dengue_climate,
                          outcome = "casos", 
                          threshold_week = i,
                          formula = f2, 
                          family = family2,
                          quantiles = quantiles,
                          len_windows = len_windows
                        )
    return(fit)
  }))
  # Model 3
  f3 <- casos ~ 1 + 
      temp_max_avg_8w +
      f(time_id, model = "rw1") +
      f(week_id, model = "rw1", cyclic = TRUE, constr = TRUE,
        group = year_id, control.group = list(model = "ar1")) +
      f(year_id, model = "iid", constr = T)
  family3 <- "poisson"
  print(paste0("Fitting model 3 for ", uf))
  results_M3 <- transpose(pblapply(start_week:(nrow(dengue_climate) - len_windows),
  function(i) {
    fit <- run_inla_model(dengue_climate,
                          outcome = "casos", 
                          threshold_week = i,
                          formula = f3, 
                          family = family3,
                          quantiles = quantiles,
                          len_windows = len_windows
                        )
    return(fit)
  }))
  # Model 4
  f4 <- casos ~ 1 + 
      temp_avg_12w +
      f(time_id, model = "rw1") +
      f(week_id, model = "rw1", constr = TRUE, cyclic = TRUE,
        group = year_id, control.group = list(model = "ar1")) +
      f(year_id, model = "iid", constr = T)
  family4 <- "poisson"
  print(paste0("Fitting model 4 for ", uf))
  results_M4 <- transpose(pblapply(start_week:(nrow(dengue_climate) - len_windows),
  function(i) {
    fit <- run_inla_model(dengue_climate, 
                          outcome = "casos",
                          threshold_week = i,
                          formula = f4, 
                          family = family4,
                          quantiles = quantiles,
                          len_windows = len_windows
                        )
    return(fit)
  }))
  # Model 5
  f5 <- casos ~ 1 + 
      temp_min_avg_12w +
      f(time_id, model = "rw1") +
      f(week_id, model = "rw1", cyclic = TRUE, constr = TRUE,
        group = year_id, control.group = list(model = "ar1")) +
      f(year_id, model = "iid", constr = T)
  family5 <- "poisson"
  print(paste0("Fitting model 5 for ", uf))
  results_M5 <- transpose(pblapply(start_week:(nrow(dengue_climate) - len_windows),
  function(i) {
    fit <- run_inla_model(dengue_climate, 
                          outcome = "casos",
                          threshold_week = i, 
                          formula = f5, 
                          family = family5,
                          quantiles = quantiles,
                          len_windows = len_windows
                        )
    return(fit)
  }))
  # Model 6
  f6 <- casos ~ 1 + 
      temp_max_avg_12w +
      f(time_id, model = "rw1") +
      f(week_id, model = "rw1", cyclic = TRUE, constr = TRUE,
        group = year_id, control.group = list(model = "ar1")) +
      f(year_id, model = "iid", constr = T)
  family6 <- "poisson"
  print(paste0("Fitting model 6 for ", uf))
  results_M6 <- transpose(pblapply(start_week:(nrow(dengue_climate) - len_windows), 
  function(i) {
    fit <- run_inla_model(dengue_climate, 
                          outcome = "casos",
                          threshold_week = i,
                          formula = f6, 
                          family = family6,
                          quantiles = quantiles,
                          len_windows = len_windows
                        )
    return(fit)
  }))
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
  print(paste0("Fitting model 7 for ", uf))
  results_M7 <- transpose(pblapply(start_week:(nrow(dengue_climate) - len_windows), 
  function(i) {
    fit <- run_inla_model(dengue_climate, 
                          outcome = "casos",
                          threshold_week = i,
                          formula = f7, 
                          family = family7,
                          quantiles = quantiles,
                          len_windows = len_windows
                        )
    return(fit)
  }))

  pred_M0 <- get_pred(results_M0$data_inla, len_windows = len_windows)
  pred_M1 <- get_pred(results_M1$data_inla, len_windows = len_windows)
  pred_M2 <- get_pred(results_M2$data_inla, len_windows = len_windows)
  pred_M3 <- get_pred(results_M3$data_inla, len_windows = len_windows)
  pred_M4 <- get_pred(results_M4$data_inla, len_windows = len_windows)
  pred_M5 <- get_pred(results_M5$data_inla, len_windows = len_windows)
  pred_M6 <- get_pred(results_M6$data_inla, len_windows = len_windows)
  pred_M7 <- get_pred(results_M7$data_inla, len_windows = len_windows)

  mae_M0 <- mean(abs(pred_M0$obs - pred_M0$predicted_cases))
  mae_M1 <- mean(abs(pred_M1$obs - pred_M1$predicted_cases))
  mae_M2 <- mean(abs(pred_M2$obs - pred_M2$predicted_cases))
  mae_M3 <- mean(abs(pred_M3$obs - pred_M3$predicted_cases))
  mae_M4 <- mean(abs(pred_M4$obs - pred_M4$predicted_cases))
  mae_M5 <- mean(abs(pred_M5$obs - pred_M5$predicted_cases))
  mae_M6 <- mean(abs(pred_M6$obs - pred_M6$predicted_cases))
  mae_M7 <- mean(abs(pred_M7$obs - pred_M7$predicted_cases))

  mae_results <- data.frame(
    file_name = c(paste0("results_M0.qs2"), 
                  paste0("results_M1.qs2"), 
                  paste0("results_M2.qs2"), 
                  paste0("results_M3.qs2"), 
                  paste0("results_M4.qs2"), 
                  paste0("results_M5.qs2"), 
                  paste0("results_M6.qs2"), 
                  paste0("results_M7.qs2")),
    mae = c(mae_M0, mae_M1, mae_M2, mae_M3, mae_M4, mae_M5, mae_M6, mae_M7)
  )

  results <- list(
    results_M0,
    results_M1,
    results_M2,
    results_M3,
    results_M4,
    results_M5,
    results_M6,
    results_M7
  )

  best_model_id <- which(mae_results$mae <= sort(mae_results$mae)[1])
  # save the results of the best model
  best_model <- mae_results$file_name[best_model_id]
  best_result <- results[best_model_id]
  qs_save(best_result,
      file = paste0("results/capital_cities/", uf, "/", best_model))
  })
