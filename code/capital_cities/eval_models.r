library(INLA)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(qs2)
library(pbapply)

source("code/aux_func.r")

states <- list(
  "AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA",
  "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", 
  "RO", "RR", "RS", "SC", "SE", "SP", "TO"
)

len_windows <- 8
quantiles <- c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975)

results <- pblapply(states, function(uf) {
  print(paste0("Evaluating models for ", uf))
  data <- read_csv(paste0("data/capital_cities/dengue_climate_", uf, "_inla.csv"), show_col_types = FALSE)
  models_list <- list.files(paste0("results/capital_cities/", uf), full.names = TRUE)
  models <- lapply(models_list, read_csv)
  model_names <- gsub(paste0("results/capital_cities/", uf), "", models_list)
  model_names <- gsub("/results_|.csv", "", model_names)
  names(models) <- model_names

  pred <- lapply(seq_along(models), function(i) {
    model <- models[[i]]
    model_name <- names(models)[i]
    model <- model %>% filter(horizon > 0)
    model <- model %>% rename(
      index = horizon,
      window = window_index,
      date_iniSE = date,
      predicted_cases = pred_mean
    )
    model <- model %>% mutate(
      lower_ci = q0.025,
      upper_ci = q0.975
    )
    model <- model %>% select(
      date_iniSE, obs, predicted_cases, lower_ci, upper_ci, index, window
    )
    pred <- model
    return(pred)
  })

  wis <- lapply(seq_along(models), function(i) {
    model <- models[[i]]
    model_name <- names(models)[i]
    model <- model %>% filter(horizon > 0)
    model <- model %>% rename(
      index = horizon,
      window = window_index,
      date_iniSE = date,
      predicted_cases = pred_mean
    )
    quant_df <- model %>% select(starts_with("q"))
    wis <- scoringutils::wis(
      model$obs,
      as.matrix(quant_df),
      quantiles
    ) %>% mean()
    return(wis)
  })

  maes <- lapply(seq_along(pred), function(j) {
    pred_j <- pred[[j]]
    mae <- mean(abs(pred_j$obs - pred_j$predicted_cases))
    return(mae)
  })

  best_model_index_mae <- which.min(unlist(maes))
  best_model_name_mae <- names(models)[best_model_index_mae]
  best_model_index_wis <- which.min(unlist(wis))
  best_model_name_wis <- names(models)[best_model_index_wis]

  df_res <- data.frame(
    uf = uf,
    best_model_mae = best_model_name_mae,
    mae = maes[[best_model_index_mae]],
    best_model_wis = best_model_name_wis,
    wis = wis[[best_model_index_wis]]
  )
  return(df_res)
})

results_df <- do.call(rbind, results)
results_df <- results_df %>% arrange(as.numeric(gsub("M", "", best_model_mae)))
write_csv(results_df, "results/capital_cities/best_models.csv")
