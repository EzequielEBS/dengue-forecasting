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
  # "PB", 
  # "PE", 
  # "PI", 
  # "PR", 
  # "RJ", 
  # "RN",
  # "RO", 
  # "RR", 
  # "RS", 
  # "SC",
  # "SE",
  # "SP",
  # "TO"
)

quantiles <- c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975)
len_windows <- 8

make_fit_for_i <- function(
  i, 
  data, 
  formula,
  family,
  quantiles = quantiles,
  len_windows = len_windows
) {
  fit <- run_inla_model(data, 
                        outcome = "casos",
                        threshold_week = i,
                        formula = formula, 
                        family = family,
                        quantiles = quantiles,
                        len_windows = len_windows
                      )
  pred_quant <- fit$fit$summary.fitted.values[
    (nrow(fit$fit$summary.fitted.values) - len_windows + 1):nrow(fit$fit$summary.fitted.values),
    3:(ncol(fit$fit$summary.fitted.values) - 1)]
  q_names <- paste0("q", formatC(quantiles, format = "f", digits = 3))
  quant_df <- as_tibble(pred_quant)
  colnames(quant_df) <- q_names

  obs <- data %>% filter(epiweek > data$epiweek[i] & epiweek <= data$epiweek[i + len_windows]) %>% 
    pull(casos)
  dates <- data %>% filter(epiweek > data$epiweek[i] & epiweek <= data$epiweek[i + len_windows]) %>%
    pull(data_iniSE)
  pred_mean <- fit$fit$summary.fitted.values[
    (nrow(fit$fit$summary.fitted.values) - len_windows + 1):nrow(fit$fit$summary.fitted.values),
    "mean"]

  out_df <- tibble(
    time_id = (i):(i + len_windows - 1),
    date = dates,
    horizon = seq_len(len_windows),
    obs = obs,
    pred_mean = pred_mean
  ) %>%
    bind_cols(quant_df)

  insamp_quant <- fit$fit$summary.fitted.values[
    1:(nrow(fit$fit$summary.fitted.values) - len_windows), 
    3:(ncol(fit$fit$summary.fitted.values) - 1)]
  ins_time_ids <- 1:(nrow(fit$fit$summary.fitted.values) - len_windows)
  ins_dates <- data %>% filter(epiweek <= data$epiweek[i]) %>% pull(data_iniSE)
  ins_obs <- data %>% filter(epiweek <= data$epiweek[i]) %>% pull(casos)
  insamp_mean <- fit$fit$summary.fitted.values[
    1:(nrow(fit$fit$summary.fitted.values) - len_windows), 
    "mean"]
  n_ins <- length(ins_time_ids)
  
  ins_q_df <- as_tibble(insamp_quant)
  colnames(ins_q_df) <- q_names

  ins_df <- tibble(
    time_id = ins_time_ids,
    date    = ins_dates,
    horizon = seq(-n_ins, -1),   # negative horizons mark insample (optional)
    obs     = ins_obs,
    pred_mean = insamp_mean
  ) %>% bind_cols(ins_q_df)

  combined_df <- bind_rows(ins_df, out_df) %>% arrange(time_id)

  return(combined_df)
}


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
  results_M0 <- pblapply(start_week:(nrow(dengue_climate) - len_windows),
    make_fit_for_i,
    data = dengue_climate,
    formula = f0,
    family = family0,
    quantiles = quantiles,
    len_windows = len_windows
  ) %>% 
    bind_rows(.id = "window_index")
  # Model 1 
  f1 <- casos ~ 1 + 
      temp_avg_8w + 
      f(time_id, model = "rw1") +
      f(week_id, model = "rw1", cyclic = TRUE, constr = TRUE,
        group = year_id, control.group = list(model = "ar1")) +
      f(year_id, model = "iid", constr = T)
  family1 <- "poisson"
  print(paste0("Fitting model 1 for ", uf))
  results_M1 <- pblapply(start_week:(nrow(dengue_climate) - len_windows),
    make_fit_for_i,
    data = dengue_climate,
    formula = f1,
    family = family1,
    quantiles = quantiles,
    len_windows = len_windows
  ) %>% 
    bind_rows(.id = "window_index")
  # Model 2
  f2 <- casos ~ 1 + 
      temp_min_avg_8w + 
      f(time_id, model = "rw1") +
      f(week_id, model = "rw1", cyclic = TRUE, constr = TRUE,
        group = year_id, control.group = list(model = "ar1")) +
      f(year_id, model = "iid", constr = T)
  family2 <- "poisson"
  print(paste0("Fitting model 2 for ", uf))
  results_M2 <- pblapply(start_week:(nrow(dengue_climate) - len_windows),
    make_fit_for_i,
    data = dengue_climate,
    formula = f2,
    family = family2,
    quantiles = quantiles,
    len_windows = len_windows
  ) %>% 
    bind_rows(.id = "window_index")
  # Model 3
  f3 <- casos ~ 1 + 
      temp_max_avg_8w +
      f(time_id, model = "rw1") +
      f(week_id, model = "rw1", cyclic = TRUE, constr = TRUE,
        group = year_id, control.group = list(model = "ar1")) +
      f(year_id, model = "iid", constr = T)
  family3 <- "poisson"
  print(paste0("Fitting model 3 for ", uf))
  results_M3 <- pblapply(start_week:(nrow(dengue_climate) - len_windows),
    make_fit_for_i,
    data = dengue_climate,
    formula = f3,
    family = family3,
    quantiles = quantiles,
    len_windows = len_windows
  ) %>% 
    bind_rows(.id = "window_index")
  # Model 4
  f4 <- casos ~ 1 + 
      temp_avg_12w +
      f(time_id, model = "rw1") +
      f(week_id, model = "rw1", constr = TRUE, cyclic = TRUE,
        group = year_id, control.group = list(model = "ar1")) +
      f(year_id, model = "iid", constr = T)
  family4 <- "poisson"
  print(paste0("Fitting model 4 for ", uf))
  results_M4 <- pblapply(start_week:(nrow(dengue_climate) - len_windows),
    make_fit_for_i,
    data = dengue_climate,
    formula = f4,
    family = family4,
    quantiles = quantiles,
    len_windows = len_windows
  ) %>% 
    bind_rows(.id = "window_index")
  # Model 5
  f5 <- casos ~ 1 + 
      temp_min_avg_12w +
      f(time_id, model = "rw1") +
      f(week_id, model = "rw1", cyclic = TRUE, constr = TRUE,
        group = year_id, control.group = list(model = "ar1")) +
      f(year_id, model = "iid", constr = T)
  family5 <- "poisson"
  print(paste0("Fitting model 5 for ", uf))
  results_M5 <- pblapply(start_week:(nrow(dengue_climate) - len_windows),
    make_fit_for_i,
    data = dengue_climate,
    formula = f5,
    family = family5,
    quantiles = quantiles,
    len_windows = len_windows
  ) %>% 
    bind_rows(.id = "window_index")
  # Model 6
  f6 <- casos ~ 1 + 
      temp_max_avg_12w +
      f(time_id, model = "rw1") +
      f(week_id, model = "rw1", cyclic = TRUE, constr = TRUE,
        group = year_id, control.group = list(model = "ar1")) +
      f(year_id, model = "iid", constr = T)
  family6 <- "poisson"
  print(paste0("Fitting model 6 for ", uf))
  results_M6 <- pblapply(start_week:(nrow(dengue_climate) - len_windows), 
    make_fit_for_i,
    data = dengue_climate,
    formula = f6,
    family = family6,
    quantiles = quantiles,
    len_windows = len_windows
  ) %>% 
    bind_rows(.id = "window_index")
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
  results_M7 <- pblapply(start_week:(nrow(dengue_climate) - len_windows), 
    make_fit_for_i,
    data = dengue_climate,
    formula = f7,
    family = family7,
    quantiles = quantiles,
    len_windows = len_windows
  ) %>% 
    bind_rows(.id = "window_index")

  # save dfs
  write_csv(results_M0, file = paste0("results/capital_cities/", uf, "/results_M0.csv"))
  write_csv(results_M1, file = paste0("results/capital_cities/", uf, "/results_M1.csv"))
  write_csv(results_M2, file = paste0("results/capital_cities/", uf, "/results_M2.csv"))
  write_csv(results_M3, file = paste0("results/capital_cities/", uf, "/results_M3.csv"))
  write_csv(results_M4, file = paste0("results/capital_cities/", uf, "/results_M4.csv"))
  write_csv(results_M5, file = paste0("results/capital_cities/", uf, "/results_M5.csv"))
  write_csv(results_M6, file = paste0("results/capital_cities/", uf, "/results_M6.csv"))
  write_csv(results_M7, file = paste0("results/capital_cities/", uf, "/results_M7.csv"))
  
})
