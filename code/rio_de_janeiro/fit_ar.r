library(tidyverse)
# library(brms)
library(cmdstanr)
library(ggplot2)
library(posterior)
library(parallel)
library(pbapply)

source("code/aux_func.r")

# Load the data
dengue_climate_rj <- read_csv("data/rio_de_janeiro/dengue_climate_rj_inla.csv")

ar_p <- cmdstan_model("code/ar_p.stan")

start_week <- 525
len_windows <- 8

ps <- seq(from = 1, to = 6, by = 1)
maes <- lapply(seq_along(ps), function(i) {
  p <- ps[i]
  cl <- makeCluster(15)
  clusterExport(cl, varlist = c("dengue_climate_rj", "ar_p", "p", "start_week", "len_windows"),
                envir = environment())

  clusterEvalQ(cl, {
    library(posterior)
    library(dplyr)
  })
  mae_window <- pblapply(start_week:(nrow(dengue_climate_rj) - len_windows),
    function(i) {
    y <- log(dengue_climate_rj[dengue_climate_rj$time_id < i, "casos"]$casos + 1)
    X <- embed(y, p + 1)
    
    standata <- list(
      N = nrow(X),
      p = p,
      y = X[,1],
      X = X[,-1, drop = FALSE],
      H = len_windows
    )
    
    fit <- ar_p$sample(
      data = standata,
      chains = 4,
      # parallel_chains = 4,
      iter_warmup = 1000,
      iter_sampling = 2500
    )
    
    pred <- ar_p$generate_quantities(
      fit,
      data = standata,
      # parallel_chains = 4
    )
    
    pred_outsample <- posterior::as_draws_df(pred$draws("y_forecast"))
    pred_outsample <- pred_outsample |>
      dplyr::mutate(dplyr::across(dplyr::starts_with("y_forecast"), ~ exp(.x) - 1)) |> 
      dplyr::select(-.chain, -.iteration, -.draw)
    
    pred_mean <- colMeans(pred_outsample)
    
    obs_values <- dengue_climate_rj %>%
      filter(time_id >= i & time_id < i + len_windows) %>%
      pull(casos)
    
    mae <- mean(abs(obs_values - pred_mean))
    return(mae)
  },
  cl = cl
  )
  stopCluster(cl)
  return(mean(unlist(mae_window)))
})

best_p <- ps[which.min(unlist(maes))]
saveRDS(best_p, file = "results/rio_de_janeiro/best_p_ar.rds")

# best model: p = 8
best_p <- readRDS(file = "results/rio_de_janeiro/best_p_ar.rds")
quantiles <- c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975)
pred_window_ar <- run_ar(p = best_p, len_windows = len_windows, start_week = start_week, 
                        data = dengue_climate_rj, quantiles = quantiles)
saveRDS(pred_window_ar, file = "results/rio_de_janeiro/results_M8.rds")
