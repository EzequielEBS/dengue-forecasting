library(Mcomp)
library(forecast)
library(TSA)
library(zoo)
library(tidyverse)
library(pbapply)
library(parallel)

dengue_climate_joinville <- read_csv("data/joinville/dengue_climate_joinville_inla.csv")
dengue_climate_joinville_ts <- ts(dengue_climate_joinville,
                                  frequency = 52, 
                                  start = c(2015, 9)
                                  )
ln100_casos <- log(dengue_climate_joinville_ts[, "casos"] + 100)

m1 <- auto.arima(window(ln100_casos, end = c(2026,7)),
                 xreg = window(dengue_climate_joinville_ts[, "temp_avg_8w"], end = c(2026,7)))
m1

cl <- makeCluster(15)
clusterExport(cl, varlist = c(
                              "dengue_climate_joinville"
                                ),
                envir = environment())
clusterEvalQ(cl, {
  library(dplyr)
})

start_week <- 514
pred_sarimax <- pblapply(start_week:(nrow(dengue_climate_joinville) - 3), function(i) {
  data_i <- ts(dengue_climate_joinville[dengue_climate_joinville$time_id <= i, ],
                frequency = 52, 
                start = c(2015, 9)
  )
  xreg <- ts(data_i[, "temp_avg_8w"],
              frequency = 52, 
              start = c(2015, 9)
  )
  y <- log(data_i[, "casos"] + 100)
  fit <- TSA::arima(y, order = c(2,1,3), seasonal = c(0,0,2), xreg = xreg)
  resids <- residuals(fit)
  pred_insample <- as.numeric(y) - as.numeric(resids)
  pred_outsample <- predict(fit, n.ahead = 3,
                  newxreg = data_i[(nrow(data_i) -2):nrow(data_i), "temp_avg_8w"])
  e95 <- qnorm(1-0.05/2,0,1)  
  e90 <- qnorm(1-0.1/2,0,1)
  e80 <- qnorm(1-0.2/2,0,1)
  e50 <- qnorm(1-0.5/2,0,1)
  rows <- length(pred_insample) + length(pred_outsample$pred)
  data_pred <- data.frame(
    data_iniSE = dengue_climate_joinville[1:rows, "data_iniSE"]$data_iniSE,
    obs = dengue_climate_joinville[1:rows, "casos"]$casos,
    predicted_cases = exp(c(pred_insample, pred_outsample$pred)) - 100,
    lower_ci = c(rep(NA, length(pred_insample)), exp(pred_outsample$pred - e95 * pred_outsample$se) - 100),
    upper_ci = c(rep(NA, length(pred_insample)), exp(pred_outsample$pred + e95 * pred_outsample$se) - 100),
    `0.025quant` = c(rep(NA, length(pred_insample)), exp(pred_outsample$pred - e95 * pred_outsample$se) - 100),
    `0.05quant`  = c(rep(NA, length(pred_insample)), exp(pred_outsample$pred - e90 * pred_outsample$se) - 100),
    `0.1quant`   = c(rep(NA, length(pred_insample)), exp(pred_outsample$pred - e80 * pred_outsample$se) - 100),
    `0.25quant`  = c(rep(NA, length(pred_insample)), exp(pred_outsample$pred - e50 * pred_outsample$se) - 100),
    `0.5quant`   = c(rep(NA, length(pred_insample)), exp(pred_outsample$pred) - 100),
    `0.75quant`  = c(rep(NA, length(pred_insample)), exp(pred_outsample$pred + e50 * pred_outsample$se) - 100),
    `0.9quant`   = c(rep(NA, length(pred_insample)), exp(pred_outsample$pred + e80 * pred_outsample$se) - 100),
    `0.95quant`  = c(rep(NA, length(pred_insample)), exp(pred_outsample$pred + e90 * pred_outsample$se) - 100),
    `0.975quant` = c(rep(NA, length(pred_insample)), exp(pred_outsample$pred + e95 * pred_outsample$se) - 100),
    check.names = FALSE
  )
  data_pred <- data_pred %>%
    mutate(predicted_cases = ifelse(predicted_cases < 0, 0, predicted_cases),
            lower_ci = ifelse(lower_ci < 0, 0, lower_ci))
  return(data_pred)
},
cl = cl
)
stopCluster(cl)

saveRDS(pred_sarimax, file = "results/joinville/results_M9.rds")
