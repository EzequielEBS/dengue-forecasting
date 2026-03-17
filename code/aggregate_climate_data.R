library(tidyverse)

climate_rj <- read_csv("data/rio_de_janeiro/climate_rj.csv")
climate_joinville <- read_csv("data/joinville/climate_joinville.csv")
climate_sc <- read_csv("data/joinville/climate_sc.csv")
climate_rj %>%
  group_by(epiweek) %>%
  summarise(geocodigo = first(geocodigo),
            temp_min_avg = mean(temp_min),
            temp_max_avg = mean(temp_max),
            temp_med_avg = mean(temp_med),
            precip_total = sum(precip_tot),
            umid_min_avg = mean(umid_min),
            umid_max_avg = mean(umid_max),
            umid_med_avg = mean(umid_med)) %>%
  write_csv("data/rio_de_janeiro/climate_rj_weekly.csv")

climate_joinville %>%
  group_by(epiweek) %>%
  summarise(geocodigo = first(geocodigo),
            temp_min_avg = mean(temp_min),
            temp_max_avg = mean(temp_max),
            temp_med_avg = mean(temp_med),
            precip_total = sum(precip_tot),
            umid_min_avg = mean(umid_min),
            umid_max_avg = mean(umid_max),
            umid_med_avg = mean(umid_med)) %>%
  write_csv("data/joinville/climate_joinville_weekly.csv")

climate_sc <- lapply(unique(climate_sc$geocodigo), function(geocodigo) {
  climate_sc %>%
    filter(geocodigo == !!geocodigo) %>%
    group_by(epiweek) %>%
    summarise(temp_min_avg = mean(temp_min),
              temp_max_avg = mean(temp_max),
              temp_med_avg = mean(temp_med),
              precip_total = sum(precip_tot),
              umid_min_avg = mean(umid_min),
              umid_max_avg = mean(umid_max),
              umid_med_avg = mean(umid_med)) %>%
    mutate(geocodigo = geocodigo)
}) %>%
  bind_rows() %>%
  write_csv("data/joinville/climate_sc_weekly.csv")
