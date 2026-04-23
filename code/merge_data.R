library(tidyverse)
library(geobr)
library(sf)

# Load the data
dengue_rj <- read_csv("data/rio_de_janeiro/dengue_rj.csv")
dengue_joinville <- read_csv("data/joinville/dengue_joinville.csv")
dengue_sc <- read_csv("data/joinville/dengue_sc.csv")
climate_rj_weekly <- read_csv("data/rio_de_janeiro/climate_rj_weekly.csv")
climate_joinville_weekly <- read_csv("data/joinville/climate_joinville_weekly.csv")
climate_sc_weekly <- read_csv("data/joinville/climate_sc_weekly.csv")
sf_sc <- read_municipality(code_muni = "SC", year = 2020)

# Merge the datasets by epiweek 
dengue_climate_rj <- dengue_rj %>%
  rename(epiweek = "SE") %>%
  left_join(climate_rj_weekly, by = "epiweek") %>%
  # select(-c(geocodigo,
  #           temp_min_avg,
  #           temp_max_avg,
  #           temp_med_avg,
  #           umid_max_avg,
  #           umid_med_avg,
  #           umid_min_avg
  #           )) %>%
  write_csv("data/rio_de_janeiro/dengue_climate_rj.csv")
dengue_climate_joinville <- dengue_joinville %>%
  rename(epiweek = "SE") %>%
  left_join(climate_joinville_weekly, by = "epiweek") %>%
  # select(-c(geocodigo,
  #           temp_min_avg,
  #           temp_max_avg,
  #           temp_med_avg,
  #           umid_max_avg,
  #           umid_med_avg,
  #           umid_min_avg
  # )) %>%
  write_csv("data/joinville/dengue_climate_joinville.csv")
dengue_climate_sc <- dengue_sc %>%
  rename(epiweek = "SE",
         geocodigo = "municipio_geocodigo") %>%
  left_join(climate_sc_weekly, by = c("geocodigo", "epiweek")) %>%
  rename(code_muni = "geocodigo") |>
  left_join(sf_sc[, c("code_muni", "geom")], by = "code_muni")

sf::st_write(dengue_climate_sc,
             dsn = "data/joinville/dengue_climate_sc.gpkg",
             layer = "dengue_climate_sc",
             delete_layer = TRUE)

# merge data for capital cities
states <- list(
  "AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA",
  "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", 
  "RO", "RR", "RS", "SC", "SE", "SP", "TO"
)

dengue_climate_capital_cities <- lapply(states, function(uf) {
  dengue_df <- read_csv(paste0("data/capital_cities/dengue_", uf, ".csv"))
  climate_df <- read_csv(paste0("data/capital_cities/climate_", uf, "_weekly.csv"))
  
  dengue_df %>%
    rename(epiweek = "SE") %>%
    left_join(climate_df, by = "epiweek") %>%
    write_csv(paste0("data/capital_cities/dengue_climate_", uf, ".csv"))
})
