library(tidyverse)
library(sf)
library(geobr)
library(patchwork)

best_models <- readr::read_csv(
  "results/capital_cities/best_models.csv",
  show_col_types = FALSE
)

pop <-  lapply(unique(best_models$uf), function(uf) {
  df <- read_csv(paste0("data/capital_cities/dengue_climate_", uf, "_inla.csv"), show_col_types = FALSE) 
  # start_week <- df[df$epiweek == 202501, "time_id"]$time_id
  df <- df %>%
    select(pop) %>%
    slice(1) %>%
    mutate(uf = uf)
}) %>%
  bind_rows()

best_models <- best_models %>%
  left_join(pop, by = "uf")

print(best_models %>% arrange(best_model_wis), n=Inf)

model_lookup <- tibble::tribble(
  ~model_id, ~model_type, ~climate_group,
  "M0",  "INLA",    "No climate variables",
  "M1",  "INLA",    "Uses climate variables",
  "M2",  "INLA",    "Uses climate variables",
  "M3",  "INLA",    "Uses climate variables",
  "M4",  "INLA",    "Uses climate variables",
  "M5",  "INLA",    "Uses climate variables",
  "M6",  "INLA",    "Uses climate variables",
  "M7",  "INLA",    "No climate variables",
  "M8",  "AR",      "No climate variables",
  "M9",  "SARIMAX", "No climate variables",
  "M10", "SARIMAX", "Uses climate variables",
  "M11", "SARIMAX", "Uses climate variables",
  "M12", "SARIMAX", "Uses climate variables",
  "M13", "SARIMAX", "Uses climate variables"
)

mean_cases_uf <- lapply(unique(best_models$uf), function(uf) {
  df <- read_csv(paste0("data/capital_cities/dengue_climate_", uf, "_inla.csv"), show_col_types = FALSE) 
  df <- df %>%
    select(casos, municipio_nome, pop)
  df$uf <- uf
  df <- df %>%
    summarise(mean_cases = mean(casos, na.rm = TRUE)) %>%
    mutate(
      uf = uf,
      municipio_nome = df$municipio_nome[1],
      mean_incidence_per_100k = mean_cases / df$pop[1] * 100000
    )
}) %>%
  bind_rows()

plot_map <- function(model_col, mae = F, cases = F) {
  best_models_plot <- best_models %>%
    mutate(model_id = .data[[model_col]]) %>%
    left_join(model_lookup, by = "model_id")

  states_sf <- geobr::read_state(year = 2020, showProgress = FALSE)
  states_sf <- states_sf %>%
    dplyr::select(
      uf = abbrev_state,
      state_name = name_state,
      geom
    ) %>%
    left_join(best_models_plot, by = "uf") %>%
    left_join(mean_cases_uf, by = "uf") %>%
    mutate(
      model_type = factor(model_type, levels = c("INLA", "AR", "SARIMAX")),
      climate_group = factor(
        climate_group,
        levels = c("Uses climate variables", "No climate variables")
      )
    )

  label_points <- sf::st_point_on_surface(states_sf)
  coords <- sf::st_coordinates(label_points)

  labels_df <- label_points %>%
    sf::st_drop_geometry() %>%
    mutate(
      X = coords[, "X"],
      Y = coords[, "Y"]
    )

  # Fetch capital cities spatial data
  capitals_sf <- geobr::read_capitals(as_sf = TRUE, showProgress = FALSE)

  plot_model_type <- ggplot(states_sf) +
    geom_sf(aes(fill = model_type), color = "white", linewidth = 0.3) +
    # geom_text(
    #   data = labels_df,
    #   aes(x = X, y = Y, label = uf),
    #   size = 4,
    #   fontface = "bold",
    #   color = "black",
    #   alpha = 0.7
    # ) +
    geom_sf(data = capitals_sf, color = "black", size = 1.5, shape = 16) +
    # Layer the capital cities (Labels)
    geom_sf_text(
      data = capitals_sf, 
      aes(label = name_muni), 
      size = 5, 
      color = "black",
      nudge_y = -0.4 # Nudges the text slightly downwards so it doesn't block the point
    ) +
    scale_fill_manual(
      values = c(
        "INLA" = "#1b9e77",
        "AR" = "#d95f02",
        "SARIMAX" = "#7570b3"
      ),
      na.value = "grey85"
    ) +
    labs(
      title = "",
      fill = ""
    ) +
    theme_void() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.text = element_text(size = 12)
    )

  plot_climate_use <- ggplot(states_sf) +
    geom_sf(aes(fill = climate_group), color = "white", linewidth = 0.3) +
    # geom_text(
    #   data = labels_df,
    #   aes(x = X, y = Y, label = uf),
    #   size = 4,
    #   fontface = "bold",
    #   color = "black"
    # ) +
    geom_sf(data = capitals_sf, color = "black", size = 1.5, shape = 16) +
    # Layer the capital cities (Labels)
    geom_sf_text(
      data = capitals_sf, 
      aes(label = name_muni), 
      size = 5, 
      color = "black",
      nudge_y = -0.4 # Nudges the text slightly downwards so it doesn't block the point
    ) +
    scale_fill_manual(
      values = c(
        "Uses climate variables" = "#2a9d8f",
        "No climate variables" = "#e9c46a"
      ),
      na.value = "grey85"
    ) +
    labs(
      title = "",
      fill = ""
    ) +
    theme_void() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.text = element_text(size = 12)
    )
  
  plot_mae <- ggplot(states_sf) +
    geom_sf(aes(fill = mae/pop*100000), color = "white", linewidth = 0.3) +
    geom_sf(data = capitals_sf, color = "black", size = 1.5, shape = 16) +
    geom_sf_text(
      data = capitals_sf, 
      aes(label = name_muni), 
      size = 5, 
      color = "black",
      nudge_y = -0.4
    ) +
    scale_fill_viridis_c(
      # limits = c(0, max(c(states_sf$mean_cases, states_sf$mae))),
      option = "C",        # good perceptual balance
      direction = -1,      # optional: darker = lower MAE (often nicer)
      na.value = "grey85",
      name = "MAE (per 100k)"
    ) +
    labs(title = "") +
    theme_void() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.text = element_text(size = 12)
    ) +
    guides(
      fill = guide_colorbar(
        barwidth = unit(15, "cm"),  # much wider scale
        barheight = unit(0.8, "cm")
      )
    )
  
  plot_cases <- ggplot(states_sf) +
    geom_sf(aes(fill = mean_incidence_per_100k), color = "white", linewidth = 0.3) +
    geom_sf(data = capitals_sf, color = "black", size = 1.5, shape = 16) +
    geom_sf_text(
      data = capitals_sf, 
      aes(label = name_muni), 
      size = 5, 
      color = "black",
      nudge_y = -0.4
    ) +
    # scale_fill_viridis_c(
    #   limits = c(0, max(c(states_sf$mean_cases, states_sf$mae))),
    #   option = "C",        # good perceptual balance
    #   direction = -1,      # optional: darker = lower MAE (often nicer)
    #   na.value = "grey85",
    #   name = "Mean Cases"
    # ) +
    scale_fill_distiller(
      palette = "YlGnBu",
      direction = 1,
      na.value = "grey85",
      name = "Mean Incidence\n(per 100k)"
    ) +
    labs(title = "") +
    theme_void() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.text = element_text(size = 12)
    ) +
    guides(
      fill = guide_colorbar(
        barwidth = unit(15, "cm"),  # much wider scale
        barheight = unit(0.8, "cm")
      )
    )
  
  if (mae * cases) {
    return(list(
      plot_model_type = plot_model_type,
      plot_climate_use = plot_climate_use,
      plot_mae = plot_mae,
      plot_cases = plot_cases
    ))
  } else {
    if (mae) {
      return(list(
        plot_model_type = plot_model_type,
        plot_climate_use = plot_climate_use,
        plot_mae = plot_mae
      ))
    } else if (cases) {
      return(list(
        plot_model_type = plot_model_type,
        plot_climate_use = plot_climate_use,
        plot_cases = plot_cases
      ))
    } else {
      return(list(
        plot_model_type = plot_model_type,
        plot_climate_use = plot_climate_use
      ))
    }
  }
}

# dir.create("results/capital_cities/plots", recursive = TRUE, showWarnings = FALSE)
plots_mae <- plot_map(model_col = "best_model_mae", mae = TRUE, cases = TRUE)
plots_wis <- plot_map(model_col = "best_model_wis")

ggsave(
  filename = file.path(
    "results/capital_cities/plots",
    paste0("best_model_type_", "best_model_mae", ".png")
  ),
  plot = plots_mae$plot_model_type,
  width = 10,
  height = 8,
  dpi = 300
)
ggsave(
  filename = file.path(
    "results/capital_cities/plots",
    paste0("best_model_type_", "best_model_wis", ".png")
  ),
  plot = plots_wis$plot_model_type,
  width = 10,
  height = 8,
  dpi = 300
)

ggsave(
  filename = file.path(
    "results/capital_cities/plots",
    paste0("best_model_climate_", "best_model_mae", ".png")
  ),
  plot = plots_mae$plot_climate_use,
  width = 10,
  height = 8,
  dpi = 300
)
ggsave(
  filename = file.path(
    "results/capital_cities/plots",
    paste0("best_model_climate_", "best_model_wis", ".png")
  ),
  plot = plots_wis$plot_climate_use,
  width = 10,
  height = 8,
  dpi = 300
)

ggsave(
  filename = file.path(
    "results/capital_cities/plots",
    paste0("best_model_mae_map.png")
  ),
  plot = plots_mae$plot_mae,
  width = 10,
  height = 8,
  dpi = 300
)
ggsave(
  filename = file.path(
    "results/capital_cities/plots",
    paste0("mean_incidence_map.png")
  ),
  plot = plots_mae$plot_cases,
  width = 10,
  height = 8,
  dpi = 300
)

plot_model_type <- (plots_mae$plot_model_type + ggtitle("MAE")) +
  (plots_wis$plot_model_type + ggtitle("WIS")) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")
plot_climate_use <- (plots_mae$plot_climate_use + ggtitle("MAE")) +
  (plots_wis$plot_climate_use + ggtitle("WIS")) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")
plot_mae_incidence <- (plots_mae$plot_mae + ggtitle("MAE (per 100k)")) +
  (plots_mae$plot_cases + ggtitle("Mean Incidence (per 100k)")) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")
plot_climate_incidence <- (plots_mae$plot_climate_use + ggtitle("MAE")) +
  (plots_mae$plot_cases + ggtitle("Mean Incidence (per 100k)")) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave(
  filename = file.path(
    "results/capital_cities/plots",
    paste0("best_model_type_combined.png")
  ),
  plot = plot_model_type,
  width = 16,
  height = 8,
  dpi = 300
)
ggsave(
  filename = file.path(
    "results/capital_cities/plots",
    paste0("best_model_climate_combined.png")
  ),
  plot = plot_climate_use,
  width = 16,
  height = 8,
  dpi = 300
)
ggsave(
  filename = file.path(
    "results/capital_cities/plots",
    paste0("mae_incidence_combined.png")
  ),
  plot = plot_mae_incidence,
  width = 16,
  height = 8,
  dpi = 300
)
ggsave(
  filename = file.path(
    "results/capital_cities/plots",
    paste0("climate_incidence_combined.png")
  ),
  plot = plot_climate_incidence,
  width = 16,
  height = 8,
  dpi = 300
)

low_high_mae <- c(
  "RR",
  "SE",
  "SP",
  "RS"
)

data_plot <- lapply(low_high_mae, function(uf) {
  read_csv(paste0("data/capital_cities/dengue_climate_", uf, "_inla.csv"), show_col_types = FALSE) %>%
    mutate(uf = uf)
}) %>%
  bind_rows()

# plot time series of some capitals
plot_low_high_mae <- ggplot(data_plot, aes(x = data_iniSE, color = uf)) +
  geom_line(aes(y = casos)) +
  labs(title = "", x = "Epidemiological Week", y = "Number of Cases") + 
  scale_x_date(date_labels = "%Y-W%V", date_breaks = "4 month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 14)
      ) +
  scale_color_manual(values = c("RR" = "red", 
                                "SE" = "green", 
                                "SP" = "orange",
                                "RS" = "purple"),
                      labels = c("RR" = "Boavista (RR)", 
                                 "SE" = "Aracaju (SE)", 
                                 "SP" = "São Paulo (SP)",
                                 "RS" = "Porto Alegre (RS)"))

ggsave(
  filename = 
    "results/capital_cities/plots/time_series_low_high_mae.png",
  plot = plot_low_high_mae,
  width = 10,
  height = 6,
  dpi = 300
)
