library(Mcomp)
library(forecast)
library(TSA)
library(zoo)
library(tidyverse)
library(parallel)
library(pbapply)

source("code/aux_func.r")
dengue_climate_joinville <- read_csv("data/joinville/dengue_climate_joinville_inla.csv")

# Choose the best model using auto.arima
start_week <- 514
i <- nrow(dengue_climate_joinville) - 3
best_par_base <- sel_par_sarimax(
  data_i = ts(dengue_climate_joinville[dengue_climate_joinville$time_id <= i, ],
            frequency = 52, 
            start = c(2015, 9)
  ),
  xreg = NULL
)
best_par_temp8w <- sel_par_sarimax(
  data_i = ts(dengue_climate_joinville[dengue_climate_joinville$time_id <= i, ],
            frequency = 52, 
            start = c(2015, 9)
  ),
  xreg = ts(dengue_climate_joinville[dengue_climate_joinville$time_id <= i, "temp_avg_8w"],
            frequency = 52, 
            start = c(2015, 9)
  )
)
best_par_temp8w_precip52w <- sel_par_sarimax(
  data_i = ts(dengue_climate_joinville[dengue_climate_joinville$time_id <= i, ],
            frequency = 52, 
            start = c(2015, 9)
  ),
  xreg = ts(dengue_climate_joinville[dengue_climate_joinville$time_id <= i, c("temp_avg_8w", "precip_avg_52w")],
            frequency = 52, 
            start = c(2015, 9)
  ),
  stepwise = F
)
best_par_temp8w_umid12w <- sel_par_sarimax(
  data_i = ts(dengue_climate_joinville[dengue_climate_joinville$time_id <= i, ],
            frequency = 52, 
            start = c(2015, 9)
  ),
  xreg = ts(dengue_climate_joinville[dengue_climate_joinville$time_id <= i, c("temp_avg_8w", "umid_max_avg_12w")],
            frequency = 52, 
            start = c(2015, 9)
  ),
  stepwise = F
)
best_par_temp12w <- sel_par_sarimax(
  data_i = ts(dengue_climate_joinville[dengue_climate_joinville$time_id <= i, ],
            frequency = 52, 
            start = c(2015, 9)
  ),
  xreg = ts(dengue_climate_joinville[dengue_climate_joinville$time_id <= i, "temp_avg_12w"],
            frequency = 52, 
            start = c(2015, 9)
  ),
  stepwise = F
)

# # choose the best model using the minimum MAE 
# p_val <- 0:3
# d_val <- 0:2
# q_val <- 0:3
# P_val <- 0:2
# D_val <- 0:1
# Q_val <- 0:2

# grid <- expand.grid(p = p_val, d = d_val, q = q_val, P = P_val, D = D_val, Q = Q_val) %>%
#   filter(!(p == 0 & d == 0 & q == 0)) %>%
#   filter(!(P == 0 & D == 0 & Q == 0))

# cl <- makeCluster(15)
# clusterExport(cl, varlist = c(
#                               "dengue_climate_joinville",
#                               "grid",
#                               "start_week",
#                               "run_sarimax"
#                               ),
#                 envir = environment())
# clusterEvalQ(cl, {
#   library(dplyr)
# })

# mae_temp8w <- lapply(1:nrow(grid), function(i) {
#   clusterExport(cl, varlist = c("i"), envir = environment())
#   mae <- pblapply(start_week:(nrow(dengue_climate_joinville) - 3), function(w) {
#     out <- tryCatch({

#       pred <- run_sarimax(
#         dengue_climate_joinville,
#         cov_name = "temp_avg_8w",
#         w = w,               
#         par = grid[i, ]
#       )

#       outsample <- (nrow(pred) - 2):nrow(pred)
#       mean(abs(pred$obs[outsample] - pred$predicted_cases[outsample]))

#     }, error = function(e) {
#       message(sprintf("Error at i=%d, w=%d: %s", i, w, e$message))
#       return(NA)
#     })

#     return(out)
#   },
#   cl = cl
#   )
#   print(sprintf("Completed i=%d/%d", i, nrow(grid)))
#   mean(unlist(mae), na.rm = TRUE)
#   }
# ) %>%
#   unlist()
# stopCluster(cl)


cl <- makeCluster(15)
clusterExport(cl, varlist = c(
                              "dengue_climate_joinville",
                              # "best_par_base",
                              # "best_par_temp8w",
                              # "best_par_temp8w_precip52w",
                              # "best_par_temp8w_umid12w",
                              "best_par_temp12w",
                              "run_sarimax"
                                ),
                envir = environment())
clusterEvalQ(cl, {
  library(dplyr)
})

start_week <- 514
pred_sarimax_base <- pblapply(start_week:(nrow(dengue_climate_joinville) - 3), function(i) {
  run_sarimax(dengue_climate_joinville, cov_name = NULL, w = i, par = best_par_base)
},
cl = cl
)
pred_sarimax_temp8w <- pblapply(start_week:(nrow(dengue_climate_joinville) - 3), function(i) {
  run_sarimax(dengue_climate_joinville, cov_name = "temp_avg_8w", w = i, par = best_par_temp8w)
},
cl = cl
)
pred_sarimax_temp8w_precip52w <- pblapply(start_week:(nrow(dengue_climate_joinville) - 3), function(i) {
  run_sarimax(dengue_climate_joinville, cov_name = c("temp_avg_8w", "precip_avg_52w"), w = i, par = best_par_temp8w_precip52w)
},
cl = cl
)
pred_sarimax_temp8w_umid12w <- pblapply(start_week:(nrow(dengue_climate_joinville) - 3), function(i) {
  run_sarimax(dengue_climate_joinville, cov_name = c("temp_avg_8w", "umid_max_avg_12w"), w = i, par = best_par_temp8w_umid12w)
},
cl = cl
)
pred_sarimax_temp12w <- pblapply(start_week:(nrow(dengue_climate_joinville) - 3), function(i) {
  run_sarimax(dengue_climate_joinville, cov_name = "temp_avg_12w", w = i, par = best_par_temp12w)
},
cl = cl
)
stopCluster(cl)

saveRDS(pred_sarimax_base, file = "results/joinville/results_M9.rds")
saveRDS(pred_sarimax_temp8w, file = "results/joinville/results_M10.rds")
saveRDS(pred_sarimax_temp8w_precip52w, file = "results/joinville/results_M11.rds")
saveRDS(pred_sarimax_temp8w_umid12w, file = "results/joinville/results_M12.rds")
saveRDS(pred_sarimax_temp12w, file = "results/joinville/results_M13.rds")
