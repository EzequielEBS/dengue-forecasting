library(INLA)
library(tidyverse)
library(ggplot2)
library(patchwork)

# Load the data
dengue_climate_rj <- read.csv("data/dengue_climate_rj.csv")
results_M1 <- readRDS("results/results_M1.rds")
results_M2 <- readRDS("results/results_M2.rds")

# Plot all predictions together
plot_all_predictions <- function(results) {
  data_pred <- data.frame()
  all_pred <- lapply(1:length(results$fit), function(i) {
    fit <- results$fit[[i]]
    obs_values <- dengue_climate_rj[which(is.na(fit$.args$data$casprov)), "casprov"]
    pred <- fit$summary.fitted.values[(nrow(fit$summary.fitted.values) - 2):nrow(fit$summary.fitted.values), ]
    upp <- pred$`0.95quant`
    low <- pred$`0.05quant`
    data_pred <<- rbind(data_pred, data.frame(obs = obs_values,pred = pred$mean, low = low, upp = upp))
  })
  data_pred$id <- 1:nrow(data_pred)
  
  ggplot(data_pred, aes(x = id)) +
    
    # Credible interval (gray ribbon)
    geom_ribbon(aes(ymin = low, ymax = upp, fill = "CI"),
                alpha = 0.6) +
    
    # Observed (bars)
    geom_col(aes(y = obs, fill = "counting"),
             alpha = 0.9) +
    
    # Fitted (dashed blue line)
    geom_line(aes(y = pred, color = "fitted"),
              linetype = "solid", linewidth = 1) +
  
    geom_vline(xintercept = seq(1, nrow(data_pred), by = 3),
             linetype = "dashed") +
    
    scale_fill_manual(name = "", values = c("counting" = "#9ecae1",
                                            "CI" = "gray70"),
                      labels = c("counting" = "Observed",
                                 "CI" = "90% CI")
    ) +
    scale_color_manual(name = "", values = c("fitted" = "blue"),
                       labels = c("Fitted")) +
    
    labs(
      x = "id",
      y = "Number of Cases"
    ) +
    
    theme_bw() +
    theme(
      panel.grid.minor = element_line(color = "gray90"),
      panel.grid.major = element_line(color = "gray80"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "top"
    )
}
plot_M1 <- plot_all_predictions(results_M1)
plot_M2 <- plot_all_predictions(results_M2)

ggsave("figures/plot_pred_M1.png", plot_M1, width = 15, height = 6)
ggsave("figures/plot_pred_M2.png", plot_M2, width = 15, height = 6)

# Plot credibility intervals for all predictions together

compute_bcis <- function(results) {
  bcis <- list()
  for (i in seq_along(results$fit)) {
    fit <- results$fit[[i]]
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

bcis_M1 <- compute_bcis(results_M1)
plot_bcis_M1 <- 
  ggplot(bcis_M1 %>% filter(term %in% c("tempmed", "umidmed")),
        aes(x = window, y = mean, color = term)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = term), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Coefficient", x = "Window") +
  theme_bw() +
  theme(legend.title = element_blank())

plot_inter_M1 <- 
  ggplot(bcis_M1 %>% filter(term %in% c("(Intercept)")),
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

bcis_M2 <- compute_bcis(results_M2)
plot_bcis_M2 <- 
  ggplot(bcis_M2 %>% filter(term %in% c("tempmed_lag5", "umidmed_lag8")),
        aes(x = window, y = mean, color = term)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = term), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Coefficient", x = "Window") +
  theme_bw() +
  theme(legend.title = element_blank())

plot_inter_M2 <- 
  ggplot(bcis_M2 %>% filter(term %in% c("(Intercept)")),
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


ggsave("vignettes/figures/plot_bcis_M1.png", plot_inter_M1 + plot_bcis_M1,
        width = 15, height = 6)
ggsave("vignettes/figures/plot_bcis_M2.png", plot_bcis_M2 + plot_inter_M2,
        width = 15, height = 6)

# summary

## compute wis
quantiles <- c(0.05, 0.1, 0.2, 0.5, 0.8, 0.9, 0.95)
compute_wis <- function(fit, quantile_level) {
  obs_values <- dengue_climate_rj[which(is.na(fit$.args$data$casprov)), "casprov"]
  pred <- fit$summary.fitted.values[(nrow(fit$summary.fitted.values) - 2):nrow(fit$summary.fitted.values), 
                                    3:(ncol(fit$summary.fitted.values) - 1)]
  pred <- as.matrix(pred)
  scoringutils::wis(obs_values, pred, quantile_level)
}

wis_M1 <- mean(sapply(results_M1$fit, compute_wis, quantile_level = quantiles))
wis_M2 <- mean(sapply(results_M2$fit, compute_wis, quantile_level = quantiles))

## compute waic
compute_waic <- function(fit) {
  fit$waic$waic
}
waic_M1 <- mean(sapply(results_M1$fit, compute_waic))
waic_M2 <- mean(sapply(results_M2$fit, compute_waic))

## compute MAE
compute_mae <- function(fit) {
  obs_values <- dengue_climate_rj[which(is.na(fit$.args$data$casprov)), "casprov"]
  pred <- fit$summary.fitted.values[(nrow(fit$summary.fitted.values) - 2):nrow(fit$summary.fitted.values), "mean"]
  mean(abs(obs_values - pred))
}
mae_M1 <- mean(sapply(results_M1$fit, compute_mae))
mae_M2 <- mean(sapply(results_M2$fit, compute_mae))

summary_results <- data.frame(
  Model = c("M1", "M2"),
  MAE = c(mae_M1, mae_M2),
  WIS = c(wis_M1, wis_M2),
  WAIC = c(waic_M1, waic_M2)
)
write.csv(summary_results, "results/summary_results.csv", row.names = FALSE)