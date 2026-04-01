run_inla_model <- function(data, outcome, threshold_week, formula, family, 
                            quantiles = c(0.025, 0.975),
                            len_windows = 3
                          ) {
  train_data <- data[data$epiweek <= data$epiweek[threshold_week], ]
  test_data <- data[data$epiweek > data$epiweek[threshold_week] &
                    data$epiweek <= data$epiweek[threshold_week + len_windows], ]
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

plot_pred_by_window <- function(data, threshold_week, ci = "95% CI") {
  plot <- ggplot(data, aes(x = data_iniSE)) +
    
    # Credible interval (gray ribbon)
    geom_ribbon(data = data |> dplyr::filter(data_iniSE > as.Date(data$data_iniSE[threshold_week])),
      aes(ymin = lower_ci, ymax = upper_ci, fill = "CI"),
                alpha = 0.9) +
    
    # Observed (bars)
    geom_line(aes(y = obs, color = "counting"),
             alpha = 0.9) +
    
    # Fitted (dashed blue line)
    geom_line(aes(y = predicted_cases, color = "fitted"),
              linetype = "dashed", linewidth = 0.6) +
    
    # geom_vline(xintercept = data$data_iniSE[threshold_week], linetype = "dashed", 
    #            color = "black", linewidth = 1.5) +
    
    scale_fill_manual(name = "", values = c("CI" = "gray70"),
                      labels = c("CI" = ci)
    ) +
    scale_color_manual(name = "", values = c("fitted" = "blue", "counting" = "orange"),
                       labels = c("counting" = "Observed",
                                  "Fitted")) +
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

plot_coef <- function(bcis, cova = T) {
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
  if (cova) {
    return(plot_inter + plot_coefs + plot_layout(guides = "collect"))
  } else {
    return(plot_inter)
  }
}

compute_wis <- function(fit, data, outcome, quantile_level) {
  obs_values <- data[[outcome]][which(is.na(fit$.args$data[[outcome]]))]
  len_windows <- length(obs_values)
  pred <- fit$summary.fitted.values[
              (nrow(fit$summary.fitted.values) - len_windows + 1):nrow(fit$summary.fitted.values), 
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

mape_by_col <- function(data_pred, col_name) {
  mape_values <- data_pred %>%
    group_by(.data[[col_name]]) %>%
    summarise(mape = mean(abs(obs - predicted_cases) / (obs)) * 100)
  return(mape_values)
}

rmse_by_col <- function(data_pred, col_name) {
  rmse_values <- data_pred %>%
    group_by(.data[[col_name]]) %>%
    summarise(rmse = sqrt(mean((obs - predicted_cases)^2)))
  return(rmse_values)
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

run_ar <- function(p, quantiles = c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975),
                   len_windows = 3, data, start_week
                  ) {
  cl <- makeCluster(15)
  clusterExport(cl, varlist = c("data", 
                                "ar_p", 
                                "p", 
                                "start_week",
                                "quantiles"
                              ),
                envir = environment())

  clusterEvalQ(cl, {
    library(posterior)
    library(dplyr)
    library(tidyr)
  })
  pred_window <- pblapply(cl = cl, start_week:(nrow(data) - len_windows), 
    function(i) {
    y <- log(data[data$time_id <= i, "casos"]$casos + 1)
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
      data = standata
      # parallel_chains = 4
    )
    
    pred_insample <- posterior::as_draws_df(pred$draws("y_rep"))
    pred_insample <- pred_insample |> 
      dplyr::select(-.chain, -.iteration, -.draw)
    pred_outsample <- posterior::as_draws_df(pred$draws("y_forecast"))
    pred_outsample <- pred_outsample |> 
      dplyr::select(-.chain, -.iteration, -.draw)
    
    pred <- cbind(pred_insample, pred_outsample)

    col_order <- colnames(pred)

    pred_summ <- pred |>
      pivot_longer(cols = everything(),
                  names_to = "variable",
                  values_to = "y") |>
      mutate(variable = factor(variable, levels = col_order)) |>
      group_by(variable) |>
      summarise(
        mean  = ifelse(mean(y) < 0, 0, mean(y)),
        lower_ci = ifelse(quantile(y, quantiles[1]) < 0, 0, quantile(y, quantiles[1])),
        upper_ci = quantile(y, quantiles[length(quantiles)]),
        .groups = "drop"
      ) |>
      mutate(mean = exp(mean) - 1,
             lower_ci = exp(lower_ci) - 1,
             upper_ci = exp(upper_ci) - 1)
    
    quant <- apply(pred, 2, function(col) {
      q <- quantile(col, probs = quantiles)
      q[1] <- ifelse(q[1] < 0, 0, q[1])
      q <- exp(q) - 1
      return(q)
    })
    quant <- quant %>%
      t() %>%
      as.data.frame()
    colnames(quant) <- paste0(quantiles, "quant")
    data <- data %>%
      filter(time_id > p & time_id <= i + len_windows
        ) %>%
      select(data_iniSE, casos) %>%
      cbind(pred_summ) %>%
      cbind(quant) %>%
      rename(obs = casos,
              predicted_cases = mean)
    return(data)
  })
  stopCluster(cl)
  return(pred_window)
}

sel_par_sarimax <- function(data_i, xreg, stepwise = F) {
  y <- log(data_i[, "casos"] + 100)
  fit <- forecast::auto.arima(y, 
                    xreg = xreg,
                    # max.p = 10, max.q = 10, max.P = 5, max.Q = 5,
                    stepwise = stepwise
                  )
  
  best_par <- data.frame(
    p = fit$arma[1],
    d = fit$arma[6],
    q = fit$arma[2],
    P = fit$arma[3],
    D = fit$arma[7],
    Q = fit$arma[4],
    m = fit$arma[5]
  )
  return(best_par)
}

run_sarimax <- function(data, cov_name, w, par, method = "CSS",
                      len_windows = 3
                      ) {
  data_i <- ts(data[data$time_id <= w, ],
                frequency = 52, 
                start = c(2015, 9)
  )
  if (is.null(cov_name)) {
    xreg <- NULL
    xnew <- NULL
  } else {
    xreg <- ts(data[data$time_id <= w, cov_name],
                frequency = 52, 
                start = c(2015, 9)
    )
    xnew <- data[data$time_id > w & data$time_id <= w + len_windows,
                  cov_name]
  }
  y <- log(data_i[, "casos"] + 100)
  fit <- forecast::Arima(y, order = c(par$p, par$d, par$q), 
                    seasonal = c(par$P, par$D, par$Q),
                    xreg = xreg,
                    method = method
                  )
  resids <- residuals(fit)
  pred_insample <- as.numeric(y) - as.numeric(resids)
  pred_outsample <- predict(fit, n.ahead = len_windows,
                  newxreg = xnew
                )
  e95 <- qnorm(1-0.05/2,0,1)  
  e90 <- qnorm(1-0.1/2,0,1)
  e80 <- qnorm(1-0.2/2,0,1)
  e50 <- qnorm(1-0.5/2,0,1)
  rows <- length(pred_insample) + length(pred_outsample$pred)
  data_pred <- data.frame(
    data_iniSE = data[1:rows, "data_iniSE"]$data_iniSE,
    obs = data[1:rows, "casos"]$casos,
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
}
