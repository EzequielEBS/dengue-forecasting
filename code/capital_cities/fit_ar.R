library(tidyverse)
library(cmdstanr)
library(ggplot2)
library(posterior)
library(parallel)
library(pbapply)
library(qs2)

source("code/aux_func.r")

states <- list(
  "AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA",
  "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", 
  "RO", "RR", "RS", "SC", "SE", "SP", "TO"
)
concluded <- list(
  # "AC",
  # "AL", 
  # "AM", 
  # "AP", 
  # "BA", 
  # "CE", 
  # "DF", 
  # "ES", 
  # "GO", 
  # "MA",
  # "MG",
  # "MS", 
  # "MT", 
  # "PA", 
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
ps <- seq(from = 3, to = 8, by = 1)
# ar_p <- cmdstan_model("code/ar_p.stan")


fetch_windows_freq <- function(uf,
                               dengue_climate,
                               ps,
                               start_week,
                               len_windows,
                               quantiles = c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975),
                               n_workers = NULL
) {
  if (is.null(n_workers)) {
    total_cores <- parallel::detectCores(logical = FALSE)
    n_workers <- max(1, total_cores - 1)
  }

  windows <- start_week:(nrow(dengue_climate) - len_windows)

  make_fit_for_i <- function(i, p) {
    # training series: all times < i
    y <- dengue_climate %>% filter(time_id < i) %>% pull(casos)
    if (length(y) < (p + 2)) {
      return(list(error = "too_short", i = i))
    }
    y_log <- log(y + 1)

    fit <- tryCatch(
      forecast::Arima(y_log, order = c(p, 0, 0), include.mean = TRUE),
      error = function(e) return(structure(list(error = conditionMessage(e)), class = "fit_error"))
    )
    if (inherits(fit, "fit_error")) return(list(error = fit$error, i = i))

    # in-sample fitted (means) and approximate predictive quantiles using residual sd
    fitted_insample <- as.numeric(fitted(fit))
    resid_sd <- sd(residuals(fit), na.rm = TRUE)
    if (is.na(resid_sd) || resid_sd == 0) resid_sd <- 1e-6

    qnorm_probs <- qnorm(quantiles)
    insamp_quant_mat <- sapply(fitted_insample, function(mu) mu + resid_sd * qnorm_probs)
    # similarly expose mean (fitted) and approximate lower/upper
    insamp_mean <- fitted_insample

    # out-of-sample: simulate n_sims trajectories of length H
    H <- len_windows

    # predictive summaries on log scale
    pred <- predict(fit, n.ahead = H)
    pred_mean_log <- as.numeric(pred$pred)
    pred_quant_log <- sapply(seq_along(pred_mean_log), function(idx) {
      mu <- pred_mean_log[idx]
      se <- pred$se[idx]
      mu + se * qnorm_probs
    })

    # back-transform
    pred_mean <- pmax(0, exp(pred_mean_log) - 1)
    pred_quant <- apply(pred_quant_log, 2, function(col) pmax(0, exp(col) - 1))

    # assemble per-horizon tibble with obs (if available)
    obs_rows <- dengue_climate %>% filter(time_id >= i & time_id < i + H)
    obs <- obs_rows$casos
    dates <- obs_rows$data_iniSE

    # name quantile cols
    q_names <- paste0("q", formatC(quantiles, format = "f", digits = 3))
    quant_df <- as_tibble(t(pred_quant))
    colnames(quant_df) <- q_names

    out_df <- tibble(
      time_id = (i):(i + H - 1),
      date = dates,
      horizon = seq_len(H),
      obs = obs,
      pred_mean = pred_mean
    ) %>%
      bind_cols(quant_df)

    # number of insample points
    n_ins <- ncol(insamp_quant_mat)        # probs x n_ins
    ins_time_ids <- (i - n_ins):(i - 1)

    # get dates/obs in the same order (match to be robust)
    ins_dates <- dengue_climate$data_iniSE[match(ins_time_ids, dengue_climate$time_id)]
    ins_obs   <- dengue_climate$casos[match(ins_time_ids, dengue_climate$time_id)]

    # back-transform log-scale summaries -> counts
    insamp_mean_back <- pmax(0, exp(insamp_mean) - 1)                     # length n_ins
    insamp_quant_back <- apply(insamp_quant_mat, 2, function(col) pmax(0, exp(col) - 1)) # probs x n_ins

    # make insample tibble
    ins_q_df <- as_tibble(t(insamp_quant_back))
    colnames(ins_q_df) <- q_names

    ins_df <- tibble(
      time_id = ins_time_ids,
      date    = ins_dates,
      horizon = seq(-n_ins, -1),   # negative horizons mark insample (optional)
      obs     = ins_obs,
      pred_mean = insamp_mean_back
    ) %>% bind_cols(ins_q_df)

    # out_df is your existing outsample tibble (horizon 1:H). Combine and sort by time_id
    combined_df <- bind_rows(ins_df, out_df) %>% arrange(time_id)

    # MAE for this window (use pred_mean vs obs)
    mae <- if (length(obs) == length(pred_mean)) mean(abs(obs - pred_mean), na.rm = TRUE) else NA_real_

    list(data = combined_df, mae = mae, i = i, p = p)
  }

  # parallel driver per p
  results_by_p <- map(ps, function(p) {
    cl <- makeCluster(n_workers)
    clusterEvalQ(cl, {
      library(forecast)
      library(dplyr)
      library(tidyr)
    })
    clusterExport(cl, c("dengue_climate", "len_windows", "quantiles", "make_fit_for_i"), envir = environment())

    # use clusterApplyLB to balance variable work
    res <- clusterApplyLB(cl, windows, function(i) make_fit_for_i(i, p))
    stopCluster(cl)

    # filter errors and collect
    errors <- keep(res, ~ !is.null(.x$error))
    if (length(errors) > 0) {
      # keep only successful results for aggregation
      res_valid <- keep(res, ~ is.null(.x$error))
    } else {
      res_valid <- res
    }

    # combine data frames for each window into one long tibble
    data_list <- map(res_valid, "data")
    combined <- bind_rows(data_list, .id = "window_index")
    mae_vals <- map_dbl(res_valid, "mae")
    avg_mae <- mean(mae_vals, na.rm = TRUE)

    list(data = combined, mae = avg_mae, raw = res, p = p)
  })

  names(results_by_p) <- paste0("p", ps)
  results_by_p
}

best_model_capital_cities <- pblapply(states, function(uf) {
  if (uf %in% concluded) {
    print(paste0("Results for ", uf, " already exist. Skipping..."))
    return(NULL)
  }
  dengue_climate <- read_csv(paste0("data/capital_cities/dengue_climate_", uf, "_inla.csv"),
                            show_col_types = FALSE)
  start_week <- dengue_climate[dengue_climate$epiweek == 202452, "time_id"]$time_id

  print(paste0("Fitting AR models for ", uf, " with different p values..."))
  time_taken <- system.time({
  results <- fetch_windows_freq(
    uf = uf,
    dengue_climate = dengue_climate,
    ps = ps,
    start_week = start_week,
    len_windows = len_windows,
    quantiles = quantiles,
    n_workers = 15
  )
  })
  print(paste0("Completed AR model fitting for ", uf, " in ", round(time_taken["elapsed"] / 60, 2), " minutes."))

  results <- transpose(results)
  maes <- results$mae
  best_p <- ps[which.min(unlist(maes))]
  best_pred <- results$data[[which.min(unlist(maes))]]
  best_pred$p <- best_p
  qs_save(best_pred, file = paste0("results/capital_cities/", uf, "/results_M8.qs2"))
  return(list(uf = uf, best_p = best_p, 
    data = best_pred,
    mae = min(unlist(maes))))
  })
