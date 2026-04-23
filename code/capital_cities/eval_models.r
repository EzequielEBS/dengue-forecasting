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
  models_list <- list.files(paste0("results/capital_cities/", uf), full.names = TRUE)
  models <- lapply(models_list, qs_read)
  model_names <- gsub(paste0("results/capital_cities/", uf), "", models_list)
  model_names <- gsub("/results_|.qs2", "", model_names)
  names(models) <- model_names

  pred <- lapply(seq_along(models), function(i) {
    model <- models[[i]]
    model_name <- names(models)[i]
    if (model_name == "M8") {
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
    } else if (model_name %in% c("M0", "M1", "M2", "M3", "M4", "M5", "M6", "M7")) {
      if (is.null(model$data_inla)) {
        pred <- get_pred(model[[1]]$data_inla, len_windows = len_windows)
      } else {
        pred <- get_pred(model$data_inla, len_windows = len_windows)
      }
    } else {
      pred <- get_pred(model, len_windows = len_windows)
    }
    return(pred)
  })

  maes <- lapply(seq_along(pred), function(j) {
    pred_j <- pred[[j]]
    mae <- mean(abs(pred_j$obs - pred_j$predicted_cases))
    return(mae)
  })

  best_model_index <- which.min(unlist(maes))
  best_model_name <- names(models)[best_model_index]

  df_res <- data.frame(
    uf = uf,
    best_model = best_model_name,
    mae = maes[[best_model_index]]
  )
  return(df_res)
})

results_df <- do.call(rbind, results)
results_df <- results_df %>% arrange(as.numeric(gsub("M", "", best_model)))
write_csv(results_df, "results/capital_cities/best_models.csv")
