library(Mcomp)
library(forecast)
library(TSA)
library(zoo)
library(tidyverse)
library(parallel)
library(pbapply)
library(qs2)

source("code/aux_func.r")
uf <- "PI"
dengue_climate <- read_csv(
  paste0("data/capital_cities/dengue_climate_", uf,"_inla.csv"),
  show_col_types = FALSE
)

# checking for stationarity
y <- ts(log(dengue_climate$casos + 100),
            frequency = 52, 
            start = c(2010, 02))

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

#q
# Acf(ts_diff)
#p
# Pacf(ts_diff)

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

best_model <- which.min(c(min(mae_base), 
                        min(mae_temp8w), 
                        min(mae_temp8w_precip52w), 
                        min(mae_temp8w_umid12w), 
                        min(mae_temp12w)))
model_names <- c("base", "temp8w", "temp8w_precip52w", "temp8w_umid12w", "temp12w")
best_model_name <- model_names[best_model]

best_par_base <- grid[which.min(mae_base), ]
best_par_temp8w <- grid[which.min(mae_temp8w), ]
best_par_temp8w_precip52w <- grid[which.min(mae_temp8w_precip52w), ]
best_par_temp8w_umid12w <- grid[which.min(mae_temp8w_umid12w), ]
best_par_temp12w <- grid[which.min(mae_temp12w), ]
if (best_model_name == "base") {
  best_par <- best_par_base
  cov_name <- NULL
  file = paste0("results/capital_cities/", uf, "/results_M9.qs2")
} else if (best_model_name == "temp8w") {
  best_par <- best_par_temp8w
  cov_name <- "temp_avg_8w"
  file = paste0("results/capital_cities/", uf, "/results_M10.qs2")
} else if (best_model_name == "temp8w_precip52w") {
  best_par <- best_par_temp8w_precip52w
  cov_name <- c("temp_avg_8w", "precip_avg_52w")
  file = paste0("results/capital_cities/", uf, "/results_M11.qs2")
} else if (best_model_name == "temp8w_umid12w") {
  best_par <- best_par_temp8w_umid12w
  cov_name <- c("temp_avg_8w", "umid_max_avg_12w")
  file = paste0("results/capital_cities/", uf, "/results_M12.qs2")
} else if (best_model_name == "temp12w") {
  best_par <- best_par_temp12w
  cov_name <- "temp_avg_12w"
  file = paste0("results/capital_cities/", uf, "/results_M13.qs2")
}

cl <- makeCluster(15)
clusterExport(cl, varlist = c(
                              "dengue_climate",
                              "best_par",
                              "cov_name",
                              "run_sarimax",
                              "len_windows"
                                ),
                envir = environment())
clusterEvalQ(cl, {
  library(dplyr)
})

pred_sarimax <- tryCatch({
  pblapply(start_week:(nrow(dengue_climate) - len_windows), 
function(i) {
  run_sarimax(dengue_climate, cov_name = cov_name, w = i, par = best_par, 
              method = "CSS-ML", len_windows = len_windows,
              optim.method = "BFGS"
              )
})}, error = function(e) {
  message(paste0("Error occurred for ", uf, ": "), 
  e$message)
  return(NULL) 
})
stopCluster(cl)

if (!is.null(pred_sarimax)) {
  qs_save(pred_sarimax, file = file)
}
