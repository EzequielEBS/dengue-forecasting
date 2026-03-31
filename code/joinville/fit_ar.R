library(tidyverse)
# library(brms)
library(cmdstanr)
library(ggplot2)
library(posterior)
library(parallel)
library(pbapply)

source("code/aux_func.r")

# Load the data
dengue_climate_joinville <- read_csv("data/joinville/dengue_climate_joinville_inla.csv")

ar_p <- cmdstan_model("code/joinville/ar_p.stan")

start_week <- 514
len_windows <- 8

ps <- seq(from = 6, to = 14, by = 1)
maes <- lapply(seq_along(ps), function(i) {
  p <- ps[i]
  cl <- makeCluster(15)
  clusterExport(cl, varlist = c("dengue_climate_joinville", "ar_p", "p", "start_week", "len_windows"),
                envir = environment())

  clusterEvalQ(cl, {
    library(posterior)
    library(dplyr)
  })
  mae_window <- pblapply(start_week:(nrow(dengue_climate_joinville) - len_windows),
    function(i) {
    y <- log(dengue_climate_joinville[dengue_climate_joinville$time_id < i, "casos"]$casos + 1)
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
    
    obs_values <- dengue_climate_joinville %>%
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
saveRDS(best_p, file = "results/joinville/best_p_ar.rds")

# best model: p = 8
quantiles <- c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975)
pred_window_ar <- run_ar(p = 8, len_windows = len_windows, start_week = start_week, 
                        data = dengue_climate_joinville, quantiles = quantiles)
saveRDS(pred_window_ar, file = "results/joinville/results_M8.rds")
