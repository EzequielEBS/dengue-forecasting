library(Mcomp)
library(forecast)
library(TSA)
library(zoo)
library(tidyverse)
library(parallel)
library(pbapply)
library(qs2)

source("code/aux_func.r")

make_fit_for_i <- function(
  data, 
  cov_name, 
  w, 
  par, 
  method = "CSS",
  len_windows = 3,
  optim.method = "BFGS",
  quantiles = c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975)
  ) {
  data_i <- ts(data[data$time_id <= w, ],
                frequency = 52, 
                start = 
  )
  if (is.null(cov_name)) {
    xreg <- NULL
    xnew <- NULL
  } else {
    xreg <- ts(data[data$time_id <= w, cov_name],
                frequency = 52, 
                start = c(as.numeric(substr(min(dengue_climate$epiweek), 1, 4)),
                        as.numeric(substr(min(dengue_climate$epiweek), 5, 6))
                    )
    )
    xnew <- data[data$time_id > w & data$time_id <= w + len_windows,
                  cov_name]
  }
  y <- log(data_i[, "casos"] + 100)
  fit <- forecast::Arima(y, order = c(par$p, par$d, par$q), 
                    seasonal = c(par$P, par$D, par$Q),
                    xreg = xreg,
                    method = method,
                    optim.method = optim.method
                  )
  resids <- residuals(fit)
  pred_insample <- as.numeric(y) - as.numeric(resids)
  pred_outsample <- predict(fit, n.ahead = len_windows,
                  newxreg = xnew
                )
  e95 <- qnorm(1-0.05/2,0,1)  
  e90 <- qnorm(1-0.1/2,0,1)
  e80 <- qnorm(1-0.2/2,0,1)
  e50 <- qnorm(1-0.5/2,0,1)

  pred_quant <- sapply(seq_along(pred_outsample$pred), function(idx) {
    mu <- pred_outsample$pred[idx]
    se <- pred_outsample$se[idx]
    exp(mu + se * c(-e95, -e90, -e80, -e50, 0, e50, e80, e90, e95)) - 100
  })
  q_names <- paste0("q", formatC(quantiles, format = "f", digits = 3))
  quant_df <- as_tibble(t(pred_quant))
  colnames(quant_df) <- q_names
  quant_df <- apply(quant_df, 2, function(col) ifelse(col < 0, 0, col)) %>% as_tibble()

  obs <- data %>% filter(epiweek > data$epiweek[w] & epiweek <= data$epiweek[w + len_windows]) %>% 
    pull(casos)
  dates <- data %>% filter(epiweek > data$epiweek[w] & epiweek <= data$epiweek[w + len_windows]) %>%
    pull(data_iniSE)
  pred_mean <- pmax(0, exp(as.numeric(pred_outsample$pred)) - 100)
  out_df <- tibble(
    time_id = (w):(w + len_windows - 1),
    date = dates,
    horizon = seq_len(len_windows),
    obs = obs,
    pred_mean = pred_mean
  ) %>%
    bind_cols(quant_df)

  insamp_quant <- sapply(seq_along(pred_insample), function(idx) {
    mu <- pred_insample[idx]
    se <- sd(resids, na.rm = TRUE)
    exp(mu + se * c(-e95, -e90, -e80, -e50, 0, e50, e80, e90, e95)) - 100
  })
  ins_time_ids <- 1:(length(pred_insample))
  ins_dates <- data %>% filter(epiweek <= data$epiweek[w]) %>% pull(data_iniSE)
  ins_obs <- data %>% filter(epiweek <= data$epiweek[w]) %>% pull(casos)
  insamp_mean <- pmax(0, exp(as.numeric(pred_insample)) - 100)
  n_ins <- length(ins_time_ids)
  ins_q_df <- as_tibble(t(insamp_quant))
  colnames(ins_q_df) <- q_names
  ins_q_df <- apply(ins_q_df, 2, function(col) ifelse(col < 0, 0, col)) %>% as_tibble()
  ins_df <- tibble(
    time_id = ins_time_ids,
    date = ins_dates,
    horizon = seq(-n_ins, -1),
    obs = ins_obs,
    pred_mean = insamp_mean
  ) %>%
    bind_cols(ins_q_df)

  combined_df <- bind_rows(ins_df, out_df) %>%
    arrange(time_id)
  return(combined_df)
}


states <- c(
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
  "RS", 
  "SC", 
  "SE", 
  "SP", 
  "TO"
)

for (uf in states) {
  print(paste0("Fitting SARIMAX for ", uf))
  
  dengue_climate <- read_csv(
    paste0("data/capital_cities/dengue_climate_", uf,"_inla.csv"),
    show_col_types = FALSE
  )

  # checking for stationarity
  y <- ts(log(dengue_climate$casos + 100),
              frequency = 52, 
              start = c(as.numeric(substr(min(dengue_climate$epiweek), 1, 4)),
                        as.numeric(substr(min(dengue_climate$epiweek), 5, 6))
                    )
          )

  d <- ndiffs(y)
  D <- nsdiffs(y, test = "hegy", max.D = 2)

  if (d > 0) {
    ts_diff <- diff(y, differences = d)
  } else {
    ts_diff <- y
  }
  if (D > 0) {
    ts_diff <- diff(ts_diff, lag = 52, differences = D)
  }

  # choose the best model using the minimum MAE
  start_week <- dengue_climate[dengue_climate$epiweek == 202452, "time_id"]$time_id
  len_windows <- 8
  p <- 0:4
  q <- 0:4
  P <- 0:1
  Q <- 0:1

  grid <- expand.grid(p = p, d = d, q = q, P = P, D = D, Q = Q) %>%
    filter(!(p == 0 & d == 0 & q == 0)) %>%
    filter(!(P == 0 & D == 0 & Q == 0))

  cl <- makeCluster(15)
  clusterExport(cl, varlist = c(
                                "dengue_climate",
                                "grid",
                                "start_week",
                                "run_sarimax",
                                "len_windows"
                                ),
                  envir = environment())
  clusterEvalQ(cl, {
    library(dplyr)
  })
  
  print(paste0("Evaluating grid of SARIMAX parameters for ", uf, " in base model..."))
  mae_base <- pblapply(1:nrow(grid), function(i) {
    mae <- lapply(start_week:(nrow(dengue_climate) - len_windows), function(w) {
      pred <- run_sarimax(
        dengue_climate,
        cov_name = NULL,
        w = w,               
        par = grid[i, ],
        len_windows = len_windows
      )

      outsample <- (nrow(pred) - len_windows+1):nrow(pred)
      mean(abs(pred$obs[outsample] - pred$predicted_cases[outsample]))
    }
    )
    return(mean(unlist(mae)))
    },
    cl = cl
  ) %>%
    unlist()
  print(paste0("Evaluating grid of SARIMAX parameters for ", uf, " in temp_avg_8w model..."))
  mae_temp8w <- pblapply(1:nrow(grid), function(i) {
    mae <- lapply(start_week:(nrow(dengue_climate) - len_windows), function(w) {
      pred <- run_sarimax(
        dengue_climate,
        cov_name = "temp_avg_8w",
        w = w,               
        par = grid[i, ],
        len_windows = len_windows
      )

      outsample <- (nrow(pred) - len_windows+1):nrow(pred)
      mean(abs(pred$obs[outsample] - pred$predicted_cases[outsample]))
    }
    )
    return(mean(unlist(mae)))
    },
    cl = cl
  ) %>%
    unlist()
  print(paste0("Evaluating grid of SARIMAX parameters for ", uf, " in temp_avg_12w model..."))
  mae_temp8w_precip52w <- pblapply(1:nrow(grid), function(i) {
    mae <- lapply(start_week:(nrow(dengue_climate) - len_windows), function(w) {
      pred <- run_sarimax(
        dengue_climate,
        cov_name = c("temp_avg_8w", "precip_avg_52w"),
        w = w,               
        par = grid[i, ],
        len_windows = len_windows
      )

      outsample <- (nrow(pred) - len_windows+1):nrow(pred)
      mean(abs(pred$obs[outsample] - pred$predicted_cases[outsample]))
    }
    )
    return(mean(unlist(mae)))
    },
    cl = cl
  ) %>%
    unlist()
  print(paste0("Evaluating grid of SARIMAX parameters for ", uf, " in temp_avg_8w + umid_max_avg_12w model..."))
  mae_temp8w_umid12w <- pblapply(1:nrow(grid), function(i) {
    mae <- lapply(start_week:(nrow(dengue_climate) - len_windows), function(w) {
      pred <- run_sarimax(
        dengue_climate,
        cov_name = c("temp_avg_8w", "umid_max_avg_12w"),
        w = w,               
        par = grid[i, ],
        len_windows = len_windows
      )

      outsample <- (nrow(pred) - len_windows+1):nrow(pred)
      mean(abs(pred$obs[outsample] - pred$predicted_cases[outsample]))
    }
    )
    return(mean(unlist(mae)))
    },
    cl = cl
  ) %>%
    unlist()
  print(paste0("Evaluating grid of SARIMAX parameters for ", uf, " in temp_avg_12w model..."))
  mae_temp12w <- pblapply(1:nrow(grid), function(i) {
    mae <- lapply(start_week:(nrow(dengue_climate) - len_windows), function(w) {
      pred <- run_sarimax(
        dengue_climate,
        cov_name = "temp_avg_12w",
        w = w,               
        par = grid[i, ],
        len_windows = len_windows
      )

      outsample <- (nrow(pred) - len_windows+1):nrow(pred)
      mean(abs(pred$obs[outsample] - pred$predicted_cases[outsample]))
    }
    )
    return(mean(unlist(mae)))
    },
    cl = cl
  ) %>%
    unlist()
  stopCluster(cl)

  best_par_base <- grid[which.min(mae_base), ]
  best_par_temp8w <- grid[which.min(mae_temp8w), ]
  best_par_temp8w_precip52w <- grid[which.min(mae_temp8w_precip52w), ]
  best_par_temp8w_umid12w <- grid[which.min(mae_temp8w_umid12w), ]
  best_par_temp12w <- grid[which.min(mae_temp12w), ]

  cl <- makeCluster(15)
  clusterExport(cl, varlist = c(
                                "dengue_climate",
                                "best_par_base",
                                "best_par_temp8w",
                                "best_par_temp8w_precip52w",
                                "best_par_temp8w_umid12w",
                                "best_par_temp12w",
                                "make_fit_for_i",
                                "start_week",
                                "len_windows"
                                  ),
                  envir = environment())
  clusterEvalQ(cl, {
    library(dplyr)
  })

  print(paste0("Fitting base SARIMAX model for ", uf, " with best parameters..."))
  pred_sarimax_base <- tryCatch({
    pblapply(start_week:(nrow(dengue_climate) - len_windows), 
      make_fit_for_i,
      data = dengue_climate,
      cov_name = NULL,
      par = best_par_base,
      method = "CSS-ML",
      len_windows = len_windows,
      optim.method = "BFGS",
      cl = cl
    ) %>% 
      bind_rows(.id = "window_index")
  }, error = function(e) {
    message(paste0("Error occurred for ", uf, " in base model: ", e$message))
    return(NULL) 
  })
  print(paste0("Fitting SARIMAX model with temp_avg_8w for ", uf, " with best parameters..."))
  pred_sarimax_temp8w <- tryCatch({
    pblapply(start_week:(nrow(dengue_climate) - len_windows), 
      make_fit_for_i,
      data = dengue_climate,
      cov_name = "temp_avg_8w",
      par = best_par_temp8w,
      method = "CSS-ML",
      len_windows = len_windows,
      optim.method = "BFGS",
      cl = cl
    ) %>% 
      bind_rows(.id = "window_index")
  }, error = function(e) {
    message(paste0("Error occurred for ", uf, " in temp8w model: ", e$message))
    return(NULL) 
  })
  print(paste0("Fitting SARIMAX model with temp_avg_8w + precip_avg_52w for ", uf, " with best parameters..."))
  pred_sarimax_temp8w_precip52w <- tryCatch({
    pblapply(start_week:(nrow(dengue_climate) - len_windows), 
      make_fit_for_i,
      data = dengue_climate,
      cov_name = c("temp_avg_8w", "precip_avg_52w"),
      par = best_par_temp8w_precip52w,
      method = "CSS-ML",
      len_windows = len_windows,
      optim.method = "BFGS",
      cl = cl
    ) %>% 
      bind_rows(.id = "window_index")
  }, error = function(e) {
    message(paste0("Error occurred for ", uf, " in temp8w_precip52w model: ", e$message))
    return(NULL) 
  })
  print(paste0("Fitting SARIMAX model with temp_avg_8w + umid_max_avg_12w for ", uf, " with best parameters..."))
  pred_sarimax_temp8w_umid12w <- tryCatch({
    pblapply(start_week:(nrow(dengue_climate) - len_windows), 
      make_fit_for_i,
      data = dengue_climate,
      cov_name = c("temp_avg_8w", "umid_max_avg_12w"),
      par = best_par_temp8w_umid12w,
      method = "CSS-ML",
      len_windows = len_windows,
      optim.method = "BFGS",
      cl = cl
    ) %>% 
      bind_rows(.id = "window_index")
  }, error = function(e) {
    message(paste0("Error occurred for ", uf, " in temp8w_umid12w model: ", e$message))
    return(NULL) 
  })
  print(paste0("Fitting SARIMAX model with temp_avg_12w for ", uf, " with best parameters..."))
  pred_sarimax_temp12w <- tryCatch({
    pblapply(start_week:(nrow(dengue_climate) - len_windows), 
      make_fit_for_i,
      data = dengue_climate,
      cov_name = "temp_avg_12w",
      par = best_par_temp12w,
      method = "CSS-ML",
      len_windows = len_windows,
      optim.method = "BFGS",
      cl = cl
    ) %>% 
      bind_rows(.id = "window_index")
  }, error = function(e) {
    message(paste0("Error occurred for ", uf, " in temp12w model: ", e$message))
    return(NULL) 
  })

  stopCluster(cl)

  if (!is.null(pred_sarimax_base)) {
    write_csv(pred_sarimax_base, paste0("results/capital_cities/results_M9", uf, ".csv"))
  }
  if (!is.null(pred_sarimax_temp8w)) {
    write_csv(pred_sarimax_temp8w, paste0("results/capital_cities/results_M10", uf, ".csv"))
  }
  if (!is.null(pred_sarimax_temp8w_precip52w)) {
    write_csv(pred_sarimax_temp8w_precip52w, paste0("results/capital_cities/results_M11", uf, ".csv"))
  }
  if (!is.null(pred_sarimax_temp8w_umid12w)) {
    write_csv(pred_sarimax_temp8w_umid12w, paste0("results/capital_cities/results_M12", uf, ".csv"))
  }
  if (!is.null(pred_sarimax_temp12w)) {
    write_csv(pred_sarimax_temp12w, paste0("results/capital_cities/results_M13", uf, ".csv"))
  }
}



