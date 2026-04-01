library(Mcomp)
library(forecast)
library(TSA)
library(zoo)
library(tidyverse)
library(parallel)
library(pbapply)

source("code/aux_func.r")
dengue_climate_rj <- read_csv("data/rio_de_janeiro/dengue_climate_rj_inla.csv")

# checking for stationarity
y <- ts(log(dengue_climate_rj$casos + 100),
            frequency = 52, 
            start = c(2014, 51))

d <- ndiffs(y)
D <- nsdiffs(y, test = "hegy", max.D = 2)

ts_diff <- diff(y, differences = d)
if (D > 0) {
  ts_diff <- diff(ts_diff, lag = 52, differences = D)
}

Acf(ts_diff)
Pacf(ts_diff)

# choose the best model using the minimum MAE
start_week <- 525
len_windows <- 8
p <- 0:4
q <- 0:7
P <- 0:1
Q <- 0:1

grid <- expand.grid(p = p, d = d, q = q, P = P, D = D, Q = Q) %>%
  filter(!(p == 0 & d == 0 & q == 0)) %>%
  filter(!(P == 0 & D == 0 & Q == 0))

cl <- makeCluster(15)
clusterExport(cl, varlist = c(
                              "dengue_climate_rj",
                              "grid",
                              "start_week",
                              "run_sarimax",
                              "len_windows"
                              ),
                envir = environment())
clusterEvalQ(cl, {
  library(dplyr)
})

mae_base <- pblapply(1:nrow(grid), function(i) {
  mae <- lapply(start_week:(nrow(dengue_climate_rj) - len_windows), function(w) {
    pred <- run_sarimax(
      dengue_climate_rj,
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

best_par_base <- grid[which.min(mae_base), ]
saveRDS(best_par_base, file = "results/rio_de_janeiro/best_par_base_sarimax.rds")

mae_temp8w <- pblapply(1:nrow(grid), function(i) {
  mae <- lapply(start_week:(nrow(dengue_climate_rj) - len_windows), function(w) {
    pred <- run_sarimax(
      dengue_climate_rj,
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

best_par_temp8w <- grid[which.min(mae_temp8w), ]
saveRDS(best_par_temp8w, file = "results/rio_de_janeiro/best_par_temp8w_sarimax.rds")

mae_temp8w_precip52w <- pblapply(1:nrow(grid), function(i) {
  mae <- lapply(start_week:(nrow(dengue_climate_rj) - len_windows), function(w) {
    pred <- run_sarimax(
      dengue_climate_rj,
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

best_par_temp8w_precip52w <- grid[which.min(mae_temp8w_precip52w), ]
saveRDS(best_par_temp8w_precip52w, file = "results/rio_de_janeiro/best_par_temp8w_precip52w_sarimax.rds")

mae_temp8w_umid12w <- pblapply(1:nrow(grid), function(i) {
  mae <- lapply(start_week:(nrow(dengue_climate_rj) - len_windows), function(w) {
    pred <- run_sarimax(
      dengue_climate_rj,
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

best_par_temp8w_umid12w <- grid[which.min(mae_temp8w_umid12w), ]
saveRDS(best_par_temp8w_umid12w, file = "results/rio_de_janeiro/best_par_temp8w_umid12w_sarimax.rds")

mae_temp12w <- pblapply(1:nrow(grid), function(i) {
  mae <- lapply(start_week:(nrow(dengue_climate_rj) - len_windows), function(w) {
    pred <- run_sarimax(
      dengue_climate_rj,
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

best_par_temp12w <- grid[which.min(mae_temp12w), ]
saveRDS(best_par_temp12w, file = "results/rio_de_janeiro/best_par_temp12w_sarimax.rds")
stopCluster(cl)

# # Choose the best model using auto.arima
# start_week <- 514
# i <- nrow(dengue_climate_rj) - 3
# best_par_base <- sel_par_sarimax(
#   data_i = ts(dengue_climate_rj[dengue_climate_rj$time_id <= i, ],
#             frequency = 52, 
#             start = c(2015, 9)
#   ),
#   xreg = NULL
# )
# best_par_temp8w <- sel_par_sarimax(
#   data_i = ts(dengue_climate_rj[dengue_climate_rj$time_id <= i, ],
#             frequency = 52, 
#             start = c(2015, 9)
#   ),
#   xreg = ts(dengue_climate_rj[dengue_climate_rj$time_id <= i, "temp_avg_8w"],
#             frequency = 52, 
#             start = c(2015, 9)
#   )
# )
# best_par_temp8w_precip52w <- sel_par_sarimax(
#   data_i = ts(dengue_climate_rj[dengue_climate_rj$time_id <= i, ],
#             frequency = 52, 
#             start = c(2015, 9)
#   ),
#   xreg = ts(dengue_climate_rj[dengue_climate_rj$time_id <= i, c("temp_avg_8w", "precip_avg_52w")],
#             frequency = 52, 
#             start = c(2015, 9)
#   ),
#   stepwise = F
# )
# best_par_temp8w_umid12w <- sel_par_sarimax(
#   data_i = ts(dengue_climate_rj[dengue_climate_rj$time_id <= i, ],
#             frequency = 52, 
#             start = c(2015, 9)
#   ),
#   xreg = ts(dengue_climate_rj[dengue_climate_rj$time_id <= i, c("temp_avg_8w", "umid_max_avg_12w")],
#             frequency = 52, 
#             start = c(2015, 9)
#   ),
#   stepwise = F
# )
# best_par_temp12w <- sel_par_sarimax(
#   data_i = ts(dengue_climate_rj[dengue_climate_rj$time_id <= i, ],
#             frequency = 52, 
#             start = c(2015, 9)
#   ),
#   xreg = ts(dengue_climate_rj[dengue_climate_rj$time_id <= i, "temp_avg_12w"],
#             frequency = 52, 
#             start = c(2015, 9)
#   ),
#   stepwise = F
# )

best_par_base <- readRDS("results/rio_de_janeiro/best_par_base_sarimax.rds")
best_par_temp8w <- readRDS("results/rio_de_janeiro/best_par_temp8w_sarimax.rds")
best_par_temp8w_precip52w <- readRDS("results/rio_de_janeiro/best_par_temp8w_precip52w_sarimax.rds")
best_par_temp8w_umid12w <- readRDS("results/rio_de_janeiro/best_par_temp8w_umid12w_sarimax.rds")
best_par_temp12w <- readRDS("results/rio_de_janeiro/best_par_temp12w_sarimax.rds")

cl <- makeCluster(15)
clusterExport(cl, varlist = c(
                              "dengue_climate_rj",
                              "best_par_base",
                              "best_par_temp8w",
                              "best_par_temp8w_precip52w",
                              "best_par_temp8w_umid12w",
                              "best_par_temp12w",
                              "run_sarimax",
                              "len_windows"
                                ),
                envir = environment())
clusterEvalQ(cl, {
  library(dplyr)
})

pred_sarimax_base <- pblapply(start_week:(nrow(dengue_climate_rj) - len_windows), 
function(i) {
  run_sarimax(dengue_climate_rj, cov_name = NULL, w = i, par = best_par_base, 
              method = "CSS-ML", len_windows = len_windows
              )
},
cl = cl
)
pred_sarimax_temp8w <- pblapply(start_week:(nrow(dengue_climate_rj) - len_windows), 
function(i) {
  run_sarimax(dengue_climate_rj, cov_name = "temp_avg_8w", w = i, 
              par = best_par_temp8w, method = "CSS-ML", len_windows = len_windows
              )
},
cl = cl
)
pred_sarimax_temp8w_precip52w <- pblapply(start_week:(nrow(dengue_climate_rj) - len_windows), 
function(i) {
  run_sarimax(dengue_climate_rj, cov_name = c("temp_avg_8w", "precip_avg_52w"), w = i, 
              par = best_par_temp8w_precip52w, method = "CSS-ML", len_windows = len_windows
              )
},
cl = cl
)
pred_sarimax_temp8w_umid12w <- pblapply(start_week:(nrow(dengue_climate_rj) - len_windows), 
function(i) {
  run_sarimax(dengue_climate_rj, cov_name = c("temp_avg_8w", "umid_max_avg_12w"), w = i, par = 
              best_par_temp8w_umid12w, method = "CSS-ML", len_windows = len_windows
              )
},
cl = cl
)
pred_sarimax_temp12w <- pblapply(start_week:(nrow(dengue_climate_rj) - len_windows), 
function(i) {
  run_sarimax(dengue_climate_rj, cov_name = "temp_avg_12w", w = i, par = best_par_temp12w,
              method = "CSS-ML", len_windows = len_windows
              )
},
cl = cl
)
stopCluster(cl)

saveRDS(pred_sarimax_base, file = "results/rio_de_janeiro/results_M9.rds")
saveRDS(pred_sarimax_temp8w, file = "results/rio_de_janeiro/results_M10.rds")
saveRDS(pred_sarimax_temp8w_precip52w, file = "results/rio_de_janeiro/results_M11.rds")
saveRDS(pred_sarimax_temp8w_umid12w, file = "results/rio_de_janeiro/results_M12.rds")
saveRDS(pred_sarimax_temp12w, file = "results/rio_de_janeiro/results_M13.rds")
