library(INLA)
library(tidyverse)
library(ggplot2)
library(patchwork)

source("code/aux_func.r")

dengue_climate_joinville <- read_csv("data/joinville/dengue_climate_joinville_inla.csv")
results_M0 <- readRDS("results/joinville/results_M0.rds")
results_M1 <- readRDS("results/joinville/results_M1.rds")
results_M2 <- readRDS("results/joinville/results_M2.rds")
results_M3 <- readRDS("results/joinville/results_M3.rds")
results_M4 <- readRDS("results/joinville/results_M4.rds")
results_M5 <- readRDS("results/joinville/results_M5.rds")
results_M6 <- readRDS("results/joinville/results_M6.rds")
results_M7 <- readRDS("results/joinville/results_M7.rds")
results_M8 <- readRDS("results/joinville/results_M8.rds")
results_M9 <- readRDS("results/joinville/results_M9.rds")
start_week <- 514

plots_M0 <- lapply(1:length(results_M0$data_inla), function(i) {
  plot_pred_by_window(results_M0$data_inla[[i]], threshold_week = start_week + (i - 1))
})
plots_M0_zoom <- lapply(1:length(results_M0$data_inla), function(i) {
  data_i <- results_M0$data_inla[[i]] |> filter(data_iniSE >= results_M0$data_inla[[i]]$data_iniSE[start_week - 59 + (i - 1)])
  plot_pred_by_window(data_i, threshold_week = nrow(data_i) - 3)
})
plots_M1 <- lapply(1:length(results_M1$data_inla), function(i) {
  plot_pred_by_window(results_M1$data_inla[[i]], threshold_week = start_week + (i - 1))
})
plots_M1_zoom <- lapply(1:length(results_M1$data_inla), function(i) {
  data_i <- results_M1$data_inla[[i]] |> filter(data_iniSE >= results_M1$data_inla[[i]]$data_iniSE[start_week - 59 + (i - 1)])
  plot_pred_by_window(data_i, threshold_week = nrow(data_i) - 3)
})
plots_M2 <- lapply(1:length(results_M2$data_inla), function(i) {
  plot_pred_by_window(results_M2$data_inla[[i]], threshold_week = start_week + (i - 1))
})
plots_M2_zoom <- lapply(1:length(results_M2$data_inla), function(i) {
  data_i <- results_M2$data_inla[[i]] |> filter(data_iniSE >= results_M2$data_inla[[i]]$data_iniSE[start_week - 59 + (i - 1)])
  plot_pred_by_window(data_i, threshold_week = nrow(data_i) - 3)
})
plots_M3 <- lapply(1:length(results_M3$data_inla), function(i) {
  plot_pred_by_window(results_M3$data_inla[[i]], threshold_week = start_week + (i - 1))
})
plots_M3_zoom <- lapply(1:length(results_M3$data_inla), function(i) {
  data_i <- results_M3$data_inla[[i]] |> filter(data_iniSE >= results_M3$data_inla[[i]]$data_iniSE[start_week - 59 + (i - 1)])
  plot_pred_by_window(data_i, threshold_week = nrow(data_i) - 3)
})
plots_M4 <- lapply(1:length(results_M4$data_inla), function(i) {
  plot_pred_by_window(results_M4$data_inla[[i]], threshold_week = start_week + (i - 1))
})
plots_M4_zoom <- lapply(1:length(results_M4$data_inla), function(i) {
  data_i <- results_M4$data_inla[[i]] |> filter(data_iniSE >= results_M4$data_inla[[i]]$data_iniSE[start_week - 59 + (i - 1)])
  plot_pred_by_window(data_i, threshold_week = nrow(data_i) - 3)
})
plots_M5 <- lapply(1:length(results_M5$data_inla), function(i) {
  plot_pred_by_window(results_M5$data_inla[[i]], threshold_week = start_week + (i - 1))
})
plots_M5_zoom <- lapply(1:length(results_M5$data_inla), function(i) {
  data_i <- results_M5$data_inla[[i]] |> filter(data_iniSE >= results_M5$data_inla[[i]]$data_iniSE[start_week - 59 + (i - 1)])
  plot_pred_by_window(data_i, threshold_week = nrow(data_i) - 3)
})
plots_M6 <- lapply(1:length(results_M6$data_inla), function(i) {
  plot_pred_by_window(results_M6$data_inla[[i]], threshold_week = start_week + (i - 1))
})
plots_M6_zoom <- lapply(1:length(results_M6$data_inla), function(i) {
  data_i <- results_M6$data_inla[[i]] |> filter(data_iniSE >= results_M6$data_inla[[i]]$data_iniSE[start_week - 59 + (i - 1)])
  plot_pred_by_window(data_i, threshold_week = nrow(data_i) - 3)
})
plots_M7 <- lapply(1:length(results_M7$data_inla), function(i) {
  plot_pred_by_window(results_M7$data_inla[[i]], threshold_week = start_week + (i - 1))
})
plots_M7_zoom <- lapply(1:length(results_M7$data_inla), function(i) {
  data_i <- results_M7$data_inla[[i]] |> filter(data_iniSE >= results_M7$data_inla[[i]]$data_iniSE[start_week - 59 + (i - 1)])
  plot_pred_by_window(data_i, threshold_week = nrow(data_i) - 3)
})
p <- 10
plots_M8 <- lapply(1:length(results_M8), function(i) {
  plot_pred_by_window(results_M8[[i]], threshold_week = start_week - (p+1) + (i - 1))
})
plots_M8_zoom <- lapply(1:length(results_M8), function(i) {
  data_i <- results_M8[[i]] |> filter(data_iniSE >= results_M8[[i]]$data_iniSE[start_week - (p+1) - 59 + (i - 1)])
  plot_pred_by_window(data_i, threshold_week = nrow(data_i) - 3)
})
plots_M9 <- lapply(1:length(results_M9), function(i) {
  data_i <- results_M9[[i]]
  plot_pred_by_window(data_i, threshold_week = start_week - 7 + (i - 1))
})
plots_M9_zoom <- lapply(1:length(results_M9), function(i) {
  data_i <- results_M9[[i]] |> filter(data_iniSE >= results_M9[[i]]$data_iniSE[start_week - 59 + (i - 1)])
  plot_pred_by_window(data_i, threshold_week = nrow(data_i) - 3)
})

saveRDS(plots_M0, file = "results/joinville/plots_pred_M0.rds")
saveRDS(plots_M1, file = "results/joinville/plots_pred_M1.rds")
saveRDS(plots_M2, file = "results/joinville/plots_pred_M2.rds")
saveRDS(plots_M3, file = "results/joinville/plots_pred_M3.rds")
saveRDS(plots_M4, file = "results/joinville/plots_pred_M4.rds")
saveRDS(plots_M5, file = "results/joinville/plots_pred_M5.rds")
saveRDS(plots_M6, file = "results/joinville/plots_pred_M6.rds")
saveRDS(plots_M7, file = "results/joinville/plots_pred_M7.rds")
saveRDS(plots_M8, file = "results/joinville/plots_pred_M8.rds")
saveRDS(plots_M9, file = "results/joinville/plots_pred_M9.rds")

saveRDS(plots_M0_zoom, file = "results/joinville/plots_pred_M0_zoom.rds")
saveRDS(plots_M1_zoom, file = "results/joinville/plots_pred_M1_zoom.rds")
saveRDS(plots_M2_zoom, file = "results/joinville/plots_pred_M2_zoom.rds")
saveRDS(plots_M3_zoom, file = "results/joinville/plots_pred_M3_zoom.rds")
saveRDS(plots_M4_zoom, file = "results/joinville/plots_pred_M4_zoom.rds")
saveRDS(plots_M5_zoom, file = "results/joinville/plots_pred_M5_zoom.rds")
saveRDS(plots_M6_zoom, file = "results/joinville/plots_pred_M6_zoom.rds")
saveRDS(plots_M7_zoom, file = "results/joinville/plots_pred_M7_zoom.rds")
saveRDS(plots_M8_zoom, file = "results/joinville/plots_pred_M8_zoom.rds")
saveRDS(plots_M9_zoom, file = "results/joinville/plots_pred_M9_zoom.rds")

pred_M0 <- get_pred(results_M0$data_inla)
pred_M1 <- get_pred(results_M1$data_inla)
pred_M2 <- get_pred(results_M2$data_inla)
pred_M3 <- get_pred(results_M3$data_inla)
pred_M4 <- get_pred(results_M4$data_inla)
pred_M5 <- get_pred(results_M5$data_inla)
pred_M6 <- get_pred(results_M6$data_inla)
pred_M7 <- get_pred(results_M7$data_inla)
pred_M8 <- get_pred(results_M8)
pred_M9 <- get_pred(results_M9)

bcis_M0 <- compute_bcis(results_M0$fit)
bcis_M1 <- compute_bcis(results_M1$fit)
bcis_M2 <- compute_bcis(results_M2$fit)
bcis_M3 <- compute_bcis(results_M3$fit)
bcis_M4 <- compute_bcis(results_M4$fit)
bcis_M5 <- compute_bcis(results_M5$fit)
bcis_M6 <- compute_bcis(results_M6$fit)
bcis_M7 <- compute_bcis(results_M7$fit)

plot_coef_M0 <- plot_coef(bcis_M0, cova = F)
plot_coef_M1 <- plot_coef(bcis_M1)
plot_coef_M2 <- plot_coef(bcis_M2)
plot_coef_M3 <- plot_coef(bcis_M3)
plot_coef_M4 <- plot_coef(bcis_M4)
plot_coef_M5 <- plot_coef(bcis_M5)
plot_coef_M6 <- plot_coef(bcis_M6)
plot_coef_M7 <- plot_coef(bcis_M7, cova = F)

ggsave("vignettes/joinville/figures/coef_M0.png", plot_coef_M0, width = 5, height = 4)
ggsave("vignettes/joinville/figures/coef_M1.png", plot_coef_M1, width = 10, height = 4)
ggsave("vignettes/joinville/figures/coef_M2.png", plot_coef_M2, width = 10, height = 4)
ggsave("vignettes/joinville/figures/coef_M3.png", plot_coef_M3, width = 10, height = 4)
ggsave("vignettes/joinville/figures/coef_M4.png", plot_coef_M4, width = 10, height = 4)
ggsave("vignettes/joinville/figures/coef_M5.png", plot_coef_M5, width = 10, height = 4)
ggsave("vignettes/joinville/figures/coef_M6.png", plot_coef_M6, width = 10, height = 4)
ggsave("vignettes/joinville/figures/coef_M7.png", plot_coef_M7, width = 5, height = 4)

plots_time_effect_M0 <- lapply(results_M0$fit, plot_random_effects, name = "time_id")
plots_time_effect_M1 <- lapply(results_M1$fit, plot_random_effects, name = "time_id")
plots_time_effect_M2 <- lapply(results_M2$fit, plot_random_effects, name = "time_id")
plots_time_effect_M3 <- lapply(results_M3$fit, plot_random_effects, name = "time_id")
plots_time_effect_M4 <- lapply(results_M4$fit, plot_random_effects, name = "time_id")
plots_time_effect_M5 <- lapply(results_M5$fit, plot_random_effects, name = "time_id")
plots_time_effect_M6 <- lapply(results_M6$fit, plot_random_effects, name = "time_id")

plots_week_effect_M0 <- lapply(results_M0$fit, plot_random_effects, name = "week_id", name_group = "year_id")
plots_week_effect_M1 <- lapply(results_M1$fit, plot_random_effects, name = "week_id", name_group = "year_id")
plots_week_effect_M2 <- lapply(results_M2$fit, plot_random_effects, name = "week_id", name_group = "year_id")
plots_week_effect_M3 <- lapply(results_M3$fit, plot_random_effects, name = "week_id", name_group = "year_id")
plots_week_effect_M4 <- lapply(results_M4$fit, plot_random_effects, name = "week_id", name_group = "year_id")
plots_week_effect_M5 <- lapply(results_M5$fit, plot_random_effects, name = "week_id", name_group = "year_id")
plots_week_effect_M6 <- lapply(results_M6$fit, plot_random_effects, name = "week_id", name_group = "year_id")
plots_week_effect_M7 <- lapply(results_M7$fit, plot_random_effects, name = "week_id")

plots_year_effect_M7 <- lapply(results_M7$fit, plot_random_effects, name = "year_id")

saveRDS(plots_time_effect_M0, file = "results/joinville/plots_time_effect_M0.rds")
saveRDS(plots_time_effect_M1, file = "results/joinville/plots_time_effect_M1.rds")
saveRDS(plots_time_effect_M2, file = "results/joinville/plots_time_effect_M2.rds")
saveRDS(plots_time_effect_M3, file = "results/joinville/plots_time_effect_M3.rds")
saveRDS(plots_time_effect_M4, file = "results/joinville/plots_time_effect_M4.rds")
saveRDS(plots_time_effect_M5, file = "results/joinville/plots_time_effect_M5.rds")
saveRDS(plots_time_effect_M6, file = "results/joinville/plots_time_effect_M6.rds")

saveRDS(plots_week_effect_M0, file = "results/joinville/plots_week_effect_M0.rds")
saveRDS(plots_week_effect_M1, file = "results/joinville/plots_week_effect_M1.rds")
saveRDS(plots_week_effect_M2, file = "results/joinville/plots_week_effect_M2.rds")
saveRDS(plots_week_effect_M3, file = "results/joinville/plots_week_effect_M3.rds")
saveRDS(plots_week_effect_M4, file = "results/joinville/plots_week_effect_M4.rds")
saveRDS(plots_week_effect_M5, file = "results/joinville/plots_week_effect_M5.rds")
saveRDS(plots_week_effect_M6, file = "results/joinville/plots_week_effect_M6.rds")
saveRDS(plots_week_effect_M7, file = "results/joinville/plots_week_effect_M7.rds")

saveRDS(plots_year_effect_M7, file = "results/joinville/plots_year_effect_M7.rds")


quantiles <- c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975)
wis_M0 <- mean(sapply(results_M0$fit, compute_wis, quantile_level = quantiles, 
                      data = dengue_climate_joinville, outcome = "casos"))
wis_M1 <- mean(sapply(results_M1$fit, compute_wis, quantile_level = quantiles, 
                      data = dengue_climate_joinville, outcome = "casos"))
wis_M2 <- mean(sapply(results_M2$fit, compute_wis, quantile_level = quantiles, 
                      data = dengue_climate_joinville, outcome = "casos"))
wis_M3 <- mean(sapply(results_M3$fit, compute_wis, quantile_level = quantiles, 
                      data = dengue_climate_joinville, outcome = "casos"))
wis_M4 <- mean(sapply(results_M4$fit, compute_wis, quantile_level = quantiles, 
                      data = dengue_climate_joinville, outcome = "casos"))
wis_M5 <- mean(sapply(results_M5$fit, compute_wis, quantile_level = quantiles, 
                      data = dengue_climate_joinville, outcome = "casos"))
wis_M6 <- mean(sapply(results_M6$fit, compute_wis, quantile_level = quantiles, 
                      data = dengue_climate_joinville, outcome = "casos"))
wis_M7 <- mean(sapply(results_M7$fit, compute_wis, quantile_level = quantiles, 
                      data = dengue_climate_joinville, outcome = "casos"))
wis_M8 <- mean(sapply(results_M8, function(pred) {
  rows_filter <- grepl("^y_forecast", as.character(pred$variable))
  pred_matrix <- as.matrix(pred[rows_filter, -c(1:6)])
  obs_values <- pred[rows_filter, "obs"]
  scoringutils::wis(obs_values, pred_matrix, quantiles)
}))
wis_M9 <- mean(sapply(results_M9, function(pred) {
  rows_filter <- (nrow(pred) - 2):(nrow(pred))
  pred_matrix <- as.matrix(pred[rows_filter, -c(1:5)])
  obs_values <- pred[rows_filter, "obs"]
  scoringutils::wis(obs_values, pred_matrix, quantiles)
}))

waic_M0 <- mean(sapply(results_M0$fit, function(fit) fit$waic$waic))
waic_M1 <- mean(sapply(results_M1$fit, function(fit) fit$waic$waic))
waic_M2 <- mean(sapply(results_M2$fit, function(fit) fit$waic$waic))
waic_M3 <- mean(sapply(results_M3$fit, function(fit) fit$waic$waic))
waic_M4 <- mean(sapply(results_M4$fit, function(fit) fit$waic$waic))
waic_M5 <- mean(sapply(results_M5$fit, function(fit) fit$waic$waic))
waic_M6 <- mean(sapply(results_M6$fit, function(fit) fit$waic$waic))
waic_M7 <- mean(sapply(results_M7$fit, function(fit) fit$waic$waic))
waic_M8 <- NA 
waic_M9 <- NA

mae_id_M0 <- mae_by_col(pred_M0, "index")
mae_id_M1 <- mae_by_col(pred_M1, "index")
mae_id_M2 <- mae_by_col(pred_M2, "index")
mae_id_M3 <- mae_by_col(pred_M3, "index")
mae_id_M4 <- mae_by_col(pred_M4, "index")
mae_id_M5 <- mae_by_col(pred_M5, "index")
mae_id_M6 <- mae_by_col(pred_M6, "index")
mae_id_M7 <- mae_by_col(pred_M7, "index")
mae_id_M8 <- mae_by_col(pred_M8, "index")
mae_id_M9 <- mae_by_col(pred_M9, "index")

rmse_id_M0 <- rmse_by_col(pred_M0, "index")
rmse_id_M1 <- rmse_by_col(pred_M1, "index")
rmse_id_M2 <- rmse_by_col(pred_M2, "index")
rmse_id_M3 <- rmse_by_col(pred_M3, "index")
rmse_id_M4 <- rmse_by_col(pred_M4, "index")
rmse_id_M5 <- rmse_by_col(pred_M5, "index")
rmse_id_M6 <- rmse_by_col(pred_M6, "index")
rmse_id_M7 <- rmse_by_col(pred_M7, "index")
rmse_id_M8 <- rmse_by_col(pred_M8, "index")
rmse_id_M9 <- rmse_by_col(pred_M9, "index")

mae_window_M0 <- mae_by_col(pred_M0, "window")
mae_window_M1 <- mae_by_col(pred_M1, "window")
mae_window_M2 <- mae_by_col(pred_M2, "window")
mae_window_M3 <- mae_by_col(pred_M3, "window")
mae_window_M4 <- mae_by_col(pred_M4, "window")
mae_window_M5 <- mae_by_col(pred_M5, "window")
mae_window_M6 <- mae_by_col(pred_M6, "window")
mae_window_M7 <- mae_by_col(pred_M7, "window")
mae_window_M8 <- mae_by_col(pred_M8, "window")
mae_window_M9 <- mae_by_col(pred_M9, "window")

rmse_window_M0 <- rmse_by_col(pred_M0, "window")
rmse_window_M1 <- rmse_by_col(pred_M1, "window")
rmse_window_M2 <- rmse_by_col(pred_M2, "window")
rmse_window_M3 <- rmse_by_col(pred_M3, "window")
rmse_window_M4 <- rmse_by_col(pred_M4, "window")
rmse_window_M5 <- rmse_by_col(pred_M5, "window")
rmse_window_M6 <- rmse_by_col(pred_M6, "window")
rmse_window_M7 <- rmse_by_col(pred_M7, "window")
rmse_window_M8 <- rmse_by_col(pred_M8, "window")
rmse_window_M9 <- rmse_by_col(pred_M9, "window")

mae_M0 <- mean(abs(pred_M0$obs - pred_M0$predicted_cases))
mae_M1 <- mean(abs(pred_M1$obs - pred_M1$predicted_cases))
mae_M2 <- mean(abs(pred_M2$obs - pred_M2$predicted_cases))
mae_M3 <- mean(abs(pred_M3$obs - pred_M3$predicted_cases))
mae_M4 <- mean(abs(pred_M4$obs - pred_M4$predicted_cases))
mae_M5 <- mean(abs(pred_M5$obs - pred_M5$predicted_cases))
mae_M6 <- mean(abs(pred_M6$obs - pred_M6$predicted_cases))
mae_M7 <- mean(abs(pred_M7$obs - pred_M7$predicted_cases))
mae_M8 <- mean(abs(pred_M8$obs - pred_M8$predicted_cases))
mae_M9 <- mean(abs(pred_M9$obs - pred_M9$predicted_cases))

rmse_M0 <- sqrt(mean((pred_M0$obs - pred_M0$predicted_cases)^2))
rmse_M1 <- sqrt(mean((pred_M1$obs - pred_M1$predicted_cases)^2))
rmse_M2 <- sqrt(mean((pred_M2$obs - pred_M2$predicted_cases)^2))
rmse_M3 <- sqrt(mean((pred_M3$obs - pred_M3$predicted_cases)^2))
rmse_M4 <- sqrt(mean((pred_M4$obs - pred_M4$predicted_cases)^2))
rmse_M5 <- sqrt(mean((pred_M5$obs - pred_M5$predicted_cases)^2))
rmse_M6 <- sqrt(mean((pred_M6$obs - pred_M6$predicted_cases)^2))
rmse_M7 <- sqrt(mean((pred_M7$obs - pred_M7$predicted_cases)^2))
rmse_M8 <- sqrt(mean((pred_M8$obs - pred_M8$predicted_cases)^2))
rmse_M9 <- sqrt(mean((pred_M9$obs - pred_M9$predicted_cases)^2))


mae_id <- rbind(
  cbind(model = "M0", mae_id_M0),
  cbind(model = "M1", mae_id_M1),
  cbind(model = "M2", mae_id_M2),
  cbind(model = "M3", mae_id_M3),
  cbind(model = "M4", mae_id_M4),
  cbind(model = "M5", mae_id_M5),
  cbind(model = "M6", mae_id_M6),
  cbind(model = "M7", mae_id_M7),
  cbind(model = "M8", mae_id_M8),
  cbind(model = "M9", mae_id_M9)
) %>%
  pivot_wider(
    names_from = model,
    values_from = mae,
    names_prefix = "mae_"
  ) %>%
  write_csv("results/joinville/mae_by_id.csv")

mae_window <- rbind(
  cbind(model = "M0", mae_window_M0),
  cbind(model = "M1", mae_window_M1),
  cbind(model = "M2", mae_window_M2),
  cbind(model = "M3", mae_window_M3),
  cbind(model = "M4", mae_window_M4),
  cbind(model = "M5", mae_window_M5),
  cbind(model = "M6", mae_window_M6),
  cbind(model = "M7", mae_window_M7),
  cbind(model = "M8", mae_window_M8),
  cbind(model = "M9", mae_window_M9)
) %>%
  pivot_wider(
    names_from = model,
    values_from = mae,
    names_prefix = "mae_"
  ) %>%
  write_csv("results/joinville/mae_by_window.csv")

rmse_id <- rbind(
  cbind(model = "M0", rmse_id_M0),
  cbind(model = "M1", rmse_id_M1),
  cbind(model = "M2", rmse_id_M2),
  cbind(model = "M3", rmse_id_M3),
  cbind(model = "M4", rmse_id_M4),
  cbind(model = "M5", rmse_id_M5),
  cbind(model = "M6", rmse_id_M6),
  cbind(model = "M7", rmse_id_M7),
  cbind(model = "M8", rmse_id_M8),
  cbind(model = "M9", rmse_id_M9)
) %>%
  pivot_wider(
    names_from = model,
    values_from = rmse,
    names_prefix = "rmse_"
  ) %>%
  write_csv("results/joinville/rmse_by_id.csv")

rmse_window <- rbind(
  cbind(model = "M0", rmse_window_M0),
  cbind(model = "M1", rmse_window_M1),
  cbind(model = "M2", rmse_window_M2),
  cbind(model = "M3", rmse_window_M3),
  cbind(model = "M4", rmse_window_M4),
  cbind(model = "M5", rmse_window_M5),
  cbind(model = "M6", rmse_window_M6),
  cbind(model = "M7", rmse_window_M7),
  cbind(model = "M8", rmse_window_M8),
  cbind(model = "M9", rmse_window_M9)
) %>%
  pivot_wider(
    names_from = model,
    values_from = rmse,
    names_prefix = "rmse_"
  ) %>%
  write_csv("results/joinville/rmse_by_window.csv")

summary_metrics <- data.frame(
  model = c("M0", "M1", "M2", "M3", "M4", "M5", "M6", "M7", "M8", "M9"),
  wis = c(wis_M0, wis_M1, wis_M2, wis_M3, wis_M4, wis_M5, wis_M6, wis_M7, wis_M8, wis_M9),
  waic = c(waic_M0, waic_M1, waic_M2, waic_M3, waic_M4, waic_M5, waic_M6, waic_M7, waic_M8, waic_M9),
  mae = c(mae_M0, mae_M1, mae_M2, mae_M3, mae_M4, mae_M5, mae_M6, mae_M7, mae_M8, mae_M9),
  rmse = c(rmse_M0, rmse_M1, rmse_M2, rmse_M3, rmse_M4, rmse_M5, rmse_M6, rmse_M7, rmse_M8, rmse_M9)
) %>%
  write_csv("results/joinville/summary_metrics.csv")
