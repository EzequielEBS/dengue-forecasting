library(INLA)
library(tidyverse)
library(ggplot2)
library(patchwork)

source("code/aux_func.r")

dengue_climate_joinville <- read_csv("data/joinville/dengue_climate_joinville_inla.csv")
results_M1 <- readRDS("results/joinville/results_M1.rds")
results_M2 <- readRDS("results/joinville/results_M2.rds")
results_M3 <- readRDS("results/joinville/results_M3.rds")
results_M4 <- readRDS("results/joinville/results_M4.rds")
results_M5 <- readRDS("results/joinville/results_M5.rds")
results_M6 <- readRDS("results/joinville/results_M6.rds")
start_week <- 255

plots_M1 <- lapply(1:length(results_M1$data_inla), function(i) {
  plot_pred_by_window(results_M1$data_inla[[i]], trashold_week = start_week + (i - 1))
})
plots_M2 <- lapply(1:length(results_M2$data_inla), function(i) {
  plot_pred_by_window(results_M2$data_inla[[i]], trashold_week = start_week + (i - 1))
})
plots_M3 <- lapply(1:length(results_M3$data_inla), function(i) {
  plot_pred_by_window(results_M3$data_inla[[i]], trashold_week = start_week + (i - 1))
})
plots_M4 <- lapply(1:length(results_M4$data_inla), function(i) {
  plot_pred_by_window(results_M4$data_inla[[i]], trashold_week = start_week + (i - 1))
})
plots_M5 <- lapply(1:length(results_M5$data_inla), function(i) {
  plot_pred_by_window(results_M5$data_inla[[i]], trashold_week = start_week + (i - 1))
})
plots_M6 <- lapply(1:length(results_M6$data_inla), function(i) {
  plot_pred_by_window(results_M6$data_inla[[i]], trashold_week = start_week + (i - 1))
})

saveRDS(plots_M1, file = "results/joinville/plots_pred_M1.rds")
saveRDS(plots_M2, file = "results/joinville/plots_pred_M2.rds")
saveRDS(plots_M3, file = "results/joinville/plots_pred_M3.rds")
saveRDS(plots_M4, file = "results/joinville/plots_pred_M4.rds")
saveRDS(plots_M5, file = "results/joinville/plots_pred_M5.rds")
saveRDS(plots_M6, file = "results/joinville/plots_pred_M6.rds")

pred_M1 <- get_pred(results_M1$data_inla)
pred_M2 <- get_pred(results_M2$data_inla)
pred_M3 <- get_pred(results_M3$data_inla)
pred_M4 <- get_pred(results_M4$data_inla)
pred_M5 <- get_pred(results_M5$data_inla)
pred_M6 <- get_pred(results_M6$data_inla)

bcis_M1 <- compute_bcis(results_M1$fit)
bcis_M2 <- compute_bcis(results_M2$fit)
bcis_M3 <- compute_bcis(results_M3$fit)
bcis_M4 <- compute_bcis(results_M4$fit)
bcis_M5 <- compute_bcis(results_M5$fit)
bcis_M6 <- compute_bcis(results_M6$fit)

plot_coef_M1 <- plot_coef(bcis_M1)
plot_coef_M2 <- plot_coef(bcis_M2)
plot_coef_M3 <- plot_coef(bcis_M3)
plot_coef_M4 <- plot_coef(bcis_M4)
plot_coef_M5 <- plot_coef(bcis_M5)
plot_coef_M6 <- plot_coef(bcis_M6)

ggsave("vignettes/joinville/figures/coef_M1.png", plot_coef_M1, width = 10, height = 4)
ggsave("vignettes/joinville/figures/coef_M2.png", plot_coef_M2, width = 10, height = 4)
ggsave("vignettes/joinville/figures/coef_M3.png", plot_coef_M3, width = 10, height = 4)
ggsave("vignettes/joinville/figures/coef_M4.png", plot_coef_M4, width = 10, height = 4)
ggsave("vignettes/joinville/figures/coef_M5.png", plot_coef_M5, width = 10, height = 4)
ggsave("vignettes/joinville/figures/coef_M6.png", plot_coef_M6, width = 10, height = 4)

plots_time_effect_M1 <- lapply(results_M1$fit, plot_random_effects, name = "time_id")
plots_time_effect_M2 <- lapply(results_M2$fit, plot_random_effects, name = "time_id")
plots_time_effect_M3 <- lapply(results_M3$fit, plot_random_effects, name = "time_id")
plots_time_effect_M4 <- lapply(results_M4$fit, plot_random_effects, name = "time_id")
plots_time_effect_M5 <- lapply(results_M5$fit, plot_random_effects, name = "time_id")
plots_time_effect_M6 <- lapply(results_M6$fit, plot_random_effects, name = "time_id")

plots_week_effect_M1 <- lapply(results_M1$fit, plot_random_effects, name = "week_id", name_group = "year_id")
plots_week_effect_M2 <- lapply(results_M2$fit, plot_random_effects, name = "week_id", name_group = "year_id")
plots_week_effect_M3 <- lapply(results_M3$fit, plot_random_effects, name = "week_id", name_group = "year_id")
plots_week_effect_M4 <- lapply(results_M4$fit, plot_random_effects, name = "week_id", name_group = "year_id")
plots_week_effect_M5 <- lapply(results_M5$fit, plot_random_effects, name = "week_id", name_group = "year_id")
plots_week_effect_M6 <- lapply(results_M6$fit, plot_random_effects, name = "week_id", name_group = "year_id")

saveRDS(plots_time_effect_M1, file = "results/joinville/plots_time_effect_M1.rds")
saveRDS(plots_time_effect_M2, file = "results/joinville/plots_time_effect_M2.rds")
saveRDS(plots_time_effect_M3, file = "results/joinville/plots_time_effect_M3.rds")
saveRDS(plots_time_effect_M4, file = "results/joinville/plots_time_effect_M4.rds")
saveRDS(plots_time_effect_M5, file = "results/joinville/plots_time_effect_M5.rds")
saveRDS(plots_time_effect_M6, file = "results/joinville/plots_time_effect_M6.rds")

saveRDS(plots_week_effect_M1, file = "results/joinville/plots_week_effect_M1.rds")
saveRDS(plots_week_effect_M2, file = "results/joinville/plots_week_effect_M2.rds")
saveRDS(plots_week_effect_M3, file = "results/joinville/plots_week_effect_M3.rds")
saveRDS(plots_week_effect_M4, file = "results/joinville/plots_week_effect_M4.rds")
saveRDS(plots_week_effect_M5, file = "results/joinville/plots_week_effect_M5.rds")
saveRDS(plots_week_effect_M6, file = "results/joinville/plots_week_effect_M6.rds")


quantiles <- c(0.05, 0.1, 0.2, 0.5, 0.8, 0.9, 0.95)
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

waic_M1 <- mean(sapply(results_M1$fit, function(fit) fit$waic$waic))
waic_M2 <- mean(sapply(results_M2$fit, function(fit) fit$waic$waic))
waic_M3 <- mean(sapply(results_M3$fit, function(fit) fit$waic$waic))
waic_M4 <- mean(sapply(results_M4$fit, function(fit) fit$waic$waic))
waic_M5 <- mean(sapply(results_M5$fit, function(fit) fit$waic$waic))
waic_M6 <- mean(sapply(results_M6$fit, function(fit) fit$waic$waic))

mae_id_M1 <- mae_by_col(pred_M1, "index")
mae_id_M2 <- mae_by_col(pred_M2, "index")
mae_id_M3 <- mae_by_col(pred_M3, "index")
mae_id_M4 <- mae_by_col(pred_M4, "index")
mae_id_M5 <- mae_by_col(pred_M5, "index")
mae_id_M6 <- mae_by_col(pred_M6, "index")

mae_window_M1 <- mae_by_col(pred_M1, "window")
mae_window_M2 <- mae_by_col(pred_M2, "window")
mae_window_M3 <- mae_by_col(pred_M3, "window")
mae_window_M4 <- mae_by_col(pred_M4, "window")
mae_window_M5 <- mae_by_col(pred_M5, "window")
mae_window_M6 <- mae_by_col(pred_M6, "window")

mae_M1 <- mean(abs(pred_M1$obs - pred_M1$predicted_cases))
mae_M2 <- mean(abs(pred_M2$obs - pred_M2$predicted_cases))
mae_M3 <- mean(abs(pred_M3$obs - pred_M3$predicted_cases))
mae_M4 <- mean(abs(pred_M4$obs - pred_M4$predicted_cases))
mae_M5 <- mean(abs(pred_M5$obs - pred_M5$predicted_cases))
mae_M6 <- mean(abs(pred_M6$obs - pred_M6$predicted_cases))

mae_id <- rbind(
  cbind(model = "M1", mae_id_M1),
  cbind(model = "M2", mae_id_M2),
  cbind(model = "M3", mae_id_M3),
  cbind(model = "M4", mae_id_M4),
  cbind(model = "M5", mae_id_M5),
  cbind(model = "M6", mae_id_M6)
) %>%
  pivot_wider(
    names_from = model,
    values_from = mae,
    names_prefix = "mae_"
  ) %>%
  write_csv("results/joinville/mae_by_id.csv")

mae_window <- rbind(
  cbind(model = "M1", mae_window_M1),
  cbind(model = "M2", mae_window_M2),
  cbind(model = "M3", mae_window_M3),
  cbind(model = "M4", mae_window_M4),
  cbind(model = "M5", mae_window_M5),
  cbind(model = "M6", mae_window_M6)
) %>%
  pivot_wider(
    names_from = model,
    values_from = mae,
    names_prefix = "mae_"
  ) %>%
  write_csv("results/joinville/mae_by_window.csv")

summary_metrics <- data.frame(
  model = c("M1", "M2", "M3", "M4", "M5", "M6"),
  wis = c(wis_M1, wis_M2, wis_M3, wis_M4, wis_M5, wis_M6),
  waic = c(waic_M1, waic_M2, waic_M3, waic_M4, waic_M5, waic_M6),
  mae = c(mae_M1, mae_M2, mae_M3, mae_M4, mae_M5, mae_M6)
) %>%
  write_csv("results/joinville/summary_metrics.csv")
