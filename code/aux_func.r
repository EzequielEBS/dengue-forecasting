run_inla_model <- function(data, outcome, trashold_week, formula, family, quantiles = c(0.025, 0.975)) {
  train_data <- data[data$epiweek <= data$epiweek[trashold_week], ]
  test_data <- data[data$epiweek > data$epiweek[trashold_week] &
                    data$epiweek <= data$epiweek[trashold_week + 3], ]
  obs_values <- c(train_data[[outcome]], test_data[[outcome]])
  test_data[[outcome]] <- NA
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

get_pred <- function(results, len_windows = 3) {
  data_pred <- do.call(rbind, lapply(1:length(results), function(i) {
    data <- results[[i]]
    data <- data %>% select(data_iniSE, obs, predicted_cases, lower_ci, upper_ci) 
    data <- data[(nrow(data) - len_windows + 1):nrow(data), ]
    index <- 1:len_windows
    data$index <- index
    data$window <- i
    return(data)
  }))
  return(data_pred)
}

plot_pred_by_window <- function(data, trashold_week) {
  plot <- ggplot(data, aes(x = data_iniSE)) +
    
    # Credible interval (gray ribbon)
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = "CI"),
                alpha = 0.6) +
    
    # Observed (bars)
    geom_col(aes(y = obs, fill = "counting"),
             alpha = 0.9) +
    
    # Fitted (dashed blue line)
    geom_line(aes(y = predicted_cases, color = "fitted"),
              linetype = "dashed", linewidth = 1) +
    
    geom_vline(xintercept = data$data_iniSE[trashold_week], linetype = "dashed", 
               color = "black", linewidth = 1.5) +
    
    scale_fill_manual(name = "", values = c("counting" = "#9ecae1",
                                            "CI" = "gray70"),
                      labels = c("counting" = "Observed",
                                 "CI" = "90% CI")
    ) +
    scale_color_manual(name = "", values = c("fitted" = "blue"),
                       labels = c("Fitted")) +
    scale_x_date(date_labels = "%Y-W%V", date_breaks = "1 month") +
    
    labs(
      x = "Epidemiological Week",
      y = "Cases",
    ) +
    
    theme_minimal() +
    theme(
      panel.grid.minor = element_line(color = "gray90"),
      panel.grid.major = element_line(color = "gray80"),
      axis.text.x = element_text(angle = 45, hjust = 1),
    )
}

compute_bcis <- function(fits) {
  bcis <- list()
  for (i in seq_along(fits)) {
    fit <- fits[[i]]
    coef <- fit$summary.fixed
    bcis[[i]] <- data.frame(
      window = i,
      term = rownames(coef),
      mean = coef$mean,
      lower = coef$`0.025quant`,
      upper = coef$`0.975quant`
    )
  }
  do.call(rbind, bcis)
}

plot_coef <- function(bcis) {
  plot_coefs <- ggplot(bcis %>% filter(!term %in% c("(Intercept)")),
        aes(x = window, y = mean, color = term)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = term), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Coefficient", x = "Window") +
  theme_bw() +
  theme(legend.title = element_blank())

  plot_inter <- 
  ggplot(bcis %>% filter(term %in% c("(Intercept)")),
        aes(x = window, y = mean, color = "inter")) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = "inter"), alpha = 0.2) +
  scale_color_manual(name = "", values = c("inter" = "blue"),
                     labels = c("Intercept")) +
  scale_fill_manual(name = "", values = c("inter" = "blue"),
                    labels = c("Intercept")) +
  # geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Intercept", x = "Window") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "none") 
  return(plot_inter + plot_coefs + plot_layout(guides = "collect"))
}

compute_wis <- function(fit, data, outcome, quantile_level) {
  obs_values <- data[[outcome]][which(is.na(fit$.args$data[[outcome]]))]
  pred <- fit$summary.fitted.values[(nrow(fit$summary.fitted.values) - 2):nrow(fit$summary.fitted.values), 
                                    3:(ncol(fit$summary.fitted.values) - 1)]
  pred <- as.matrix(pred)
  scoringutils::wis(obs_values, pred, quantile_level)
}

mae_by_col <- function(data_pred, col_name) {
  mae_values <- data_pred %>%
    group_by(.data[[col_name]]) %>%
    summarise(mae = mean(abs(obs - predicted_cases)))
  return(mae_values)
}


plot_random_effects <- function(fit, name, name_group = NULL){
  if (is.null(name_group)) {
    random_effects <- fit$summary.random[[name]]
    plot <- ggplot(random_effects, aes(x = ID, y = mean)) +
      geom_line() +
      geom_ribbon(aes(ymin = `0.025quant`, ymax = `0.975quant`), alpha = 0.2) +
      labs(title = paste("Random Effects -", name), x = "ID", y = "Mean with 95% CI") +
      theme_minimal()
  } else {
    random_effects <- fit$summary.random[[name]]
    random_effects_grouped <- random_effects %>%
      # ensure numeric ID (handles factors/strings)
      dplyr::mutate(ID_num = ID) %>%
      # start group=1 and increment whenever ID decreases or repeats relative to previous row
      dplyr::mutate(group = 1 + cumsum(ID_num <= lag(ID_num, default = -Inf)),
             group_id = paste0("g", group, "_id", ID_num)) %>%
      dplyr::select(-ID_num)
    plot <- ggplot(random_effects_grouped, aes(x = ID, y = mean, color = as.factor(group))) +
      geom_line() +
      # geom_ribbon(aes(ymin = `0.025quant`, ymax = `0.975quant`), alpha = 0.2) +
      labs(title = paste("Random Effects -", name), x = "ID", y = "Mean with 95% CI") +
      scale_color_manual(name = name_group,
        values = 
        viridis::viridis(length(unique(random_effects_grouped$group)), option = "D")) +
      theme_minimal()
  }
  return(plot)
}
