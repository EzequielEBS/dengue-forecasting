run_inla_model <- function(data, trashold_week, formula, family, quantiles = c(0.025, 0.975)) {
  train_data <- data[data$epiweek <= data$epiweek[trashold_week], ]
  test_data <- data[data$epiweek > data$epiweek[trashold_week] &
                    data$epiweek <= data$epiweek[trashold_week + 3], ]
  obs_values <- c(train_data$casprov, test_data$casprov)
  test_data$casprov <- NA
  data_inla <- rbind(train_data, test_data)
  
  fit <-  inla(formula,
               data = data_inla,
               family = family,
               control.predictor = list(compute = TRUE,
                                        link = 1,
                                        quantiles = quantiles
                                        ),
               control.compute = list(dic = TRUE, 
                                      waic = TRUE,
                                      cpo = TRUE
                                    )
  )
  
  data_inla$obs <- obs_values
  data_inla$predicted_cases <- fit$summary.fitted.values$mean
  low_name <- paste0(quantiles[1], "quant")
  upp_name <- paste0(quantiles[length(quantiles)], "quant")
  data_inla$lower_ci <- fit$summary.fitted.values[[low_name]]
  data_inla$upper_ci <- fit$summary.fitted.values[[upp_name]]
  data_inla$data_iniSE <- as.Date(data_inla$data_iniSE)
  return(list(fit = fit, data_inla = data_inla))
}