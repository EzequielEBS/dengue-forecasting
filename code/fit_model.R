library(INLA)
library(tidyverse)
library(ggplot2)
library(purrr)

# https://www.paulamoraga.com/book-geospatial/sec-inla.html

# Load the data
dengue_climate_rj <- read.csv("data/dengue_climate_rj.csv")

# Prepare the data for INLA
dengue_climate_rj$tempmed_lag5 <- lag(dengue_climate_rj$tempmed, 5)
dengue_climate_rj$umidmed_lag8 <- lag(dengue_climate_rj$umidmed, 8)
dengue_climate_rj <- dengue_climate_rj[!is.na(dengue_climate_rj$tempmed_lag5) & 
                                         !is.na(dengue_climate_rj$umidmed_lag8), ]
dengue_climate_rj <- dengue_climate_rj %>%
  mutate(year = as.numeric(substr(epiweek, 1, 4)))
dengue_climate_rj$week_id <- as.numeric(substr(dengue_climate_rj$epiweek, 5, 6))
dengue_climate_rj$time_id <- as.numeric(factor(dengue_climate_rj$epiweek))
dengue_climate_rj$year_id <- as.numeric(factor(dengue_climate_rj$year))
dengue_climate_rj$obs_id <- 1:nrow(dengue_climate_rj) 

run_inla_model <- function(trashold_week, formula) {
  train_data <- dengue_climate_rj[dengue_climate_rj$epiweek <= dengue_climate_rj$epiweek[trashold_week], ]
  test_data <- dengue_climate_rj[dengue_climate_rj$epiweek > dengue_climate_rj$epiweek[trashold_week] &
                                 dengue_climate_rj$epiweek <= dengue_climate_rj$epiweek[trashold_week + 3], ]
  obs_values <- c(train_data$casprov, test_data$casprov)
  test_data$casprov <- NA
  data_inla <- rbind(train_data, test_data)
  
  fit <-  inla(formula,
               data = data_inla,
               family = "nbinomial",
               control.predictor = list(compute = TRUE,
                                        link = 1),
               control.compute = list(dic = TRUE, waic = TRUE)
  )
  
  data_inla$obs <- obs_values
  data_inla$predicted_cases <- fit$summary.fitted.values$mean
  data_inla$lower_ci <- fit$summary.fitted.values$`0.025quant`
  data_inla$upper_ci <- fit$summary.fitted.values$`0.975quant`
  data_inla$data_iniSE <- as.Date(data_inla$data_iniSE)
  
  plot <- ggplot(data_inla, aes(x = data_iniSE)) +
    
    # Credible interval (gray ribbon)
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = "CI"),
                alpha = 0.6) +
    
    # Observed (bars)
    geom_col(aes(y = obs, fill = "counting"),
             alpha = 0.9) +
    
    # Fitted (dashed blue line)
    geom_line(aes(y = predicted_cases, color = "fitted"),
              linetype = "dashed", linewidth = 1) +
    
    geom_vline(xintercept = data_inla$data_iniSE[i], linetype = "dashed", 
               color = "black", linewidth = 1.5) +
    
    scale_fill_manual(name = "", values = c("counting" = "#9ecae1",
                                            "CI" = "gray70"),
                      labels = c("counting" = "Observed",
                                 "CI" = "95% CI")
    ) +
    scale_color_manual(name = "", values = c("fitted" = "blue"),
                       labels = c("Fitted")) +
    scale_x_date(date_labels = "%Y-W%V", date_breaks = "1 month") +
    
    labs(
      x = "Epidemiological Week",
      y = "Number of Cases",
    ) +
    
    theme_minimal() +
    theme(
      panel.grid.minor = element_line(color = "gray90"),
      panel.grid.major = element_line(color = "gray80"),
      axis.text.x = element_text(angle = 45, hjust = 1),
    )
  return(list(fit = fit, plot = plot))
}

f1 <- casprov ~ 1 + 
    tempmed + umidmed +
    f(time_id, model = "rw1")

result_M1 <- transpose(lapply(111:(nrow(dengue_climate_rj) - 3), function(i) {
  run_inla_model(i, f1)
}))
