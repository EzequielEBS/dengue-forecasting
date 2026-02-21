library(tidyverse)

climate_rj <- read_csv("data/climate_rj.csv")
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
  write_csv("data/climate_rj_weekly.csv")
  