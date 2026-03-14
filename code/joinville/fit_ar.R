library(tidyverse)
# library(brms)
library(cmdstanr)
library(ggplot2)
library(posterior)
library(parallel)

source("code/aux_func.r")

# Load the data
dengue_climate_joinville <- read_csv("data/joinville/dengue_climate_joinville_inla.csv")

ar_p <- cmdstan_model("code/joinville/ar_p.stan")

start_week <- 514

ps <- seq(from = 6, to = 14, by = 1)
maes <- lapply(seq_along(ps), function(i) {
  p <- ps[i]
  cl <- makeCluster(15)
  clusterExport(cl, varlist = c("dengue_climate_joinville", "ar_p", "p", "start_week"),
                envir = environment())

  clusterEvalQ(cl, {
    library(posterior)
    library(dplyr)
  })
  mse_window <- parLapply(cl, start_week:(nrow(dengue_climate_joinville) - 3), function(i) {
    y <- log(dengue_climate_joinville[dengue_climate_joinville$time_id < i, "casos"]$casos + 1)
    X <- embed(y, p + 1)
    
    standata <- list(
      N = nrow(X),
      p = p,
      y = X[,1],
      X = X[,-1, drop = FALSE],
      H = 3
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
      parallel_chains = 4
    )
    
    pred_outsample <- posterior::as_draws_df(pred$draws("y_forecast"))
    pred_outsample <- pred_outsample |>
      dplyr::mutate(dplyr::across(dplyr::starts_with("y_forecast"), ~ exp(.x) - 1)) |> 
      dplyr::select(-.chain, -.iteration, -.draw)
    
    pred_mean <- colMeans(pred_outsample)
    
    obs_values <- dengue_climate_joinville %>%
      filter(time_id >= i & time_id < i + 3) %>%
      pull(casos)
    
    mae <- mean(abs(obs_values - pred_mean))
    return(mae)
  })
  stopCluster(cl)
  return(mean(unlist(mse_window)))
})

# best model: p = 10
quantiles <- c(0.05, 0.1, 0.2, 0.5, 0.8, 0.9, 0.95)
pred_window_ar <- run_ar(p = 10)
saveRDS(pred_window_ar, file = "results/joinville/results_M8.rds")
