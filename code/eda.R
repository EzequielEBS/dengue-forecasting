library(tidyverse)
library(ggplot2)
library(corrplot)

# Load the merged dataset
dengue_climate_rj <- read_csv("data/dengue_climate_rj.csv")

# Prepare the data

# Exploratory Data Analysis (EDA)

# Time series plot of dengue cases
ggplot() +
  geom_line(data = dengue_climate_rj, 
            aes(x = data_iniSE, y = casprov, color = "Probable Cases")) +
  geom_line(data = dengue_climate_rj, 
            aes(x = data_iniSE, y = casos, color = "Reported Cases")) +
  labs(title = "", x = "Epidemiological Week", y = "Number of Cases") + 
  scale_x_date(date_labels = "%Y-W%V", date_breaks = "1 month") +
  scale_color_manual(values = c("Reported Cases" = "blue",
                                "Probable Cases" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(),
        legend.position = "top")

# Time series plot of climate variables
ggplot(dengue_climate_rj, aes(x = data_iniSE)) +
  geom_line(aes(y = tempmed)) +
  labs(title = "", x = "Epidemiological Week", y = "Average Temperature (°C)") +
  scale_x_date(date_labels = "%Y-W%V", date_breaks = "1 month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(),
        )

ggplot(dengue_climate_rj, aes(x = data_iniSE)) +
  geom_line(aes(y = umidmed)) +
  labs(title = "", x = "Epidemiological Week", y = "Average Relative Humidity (%)") +
  scale_x_date(date_labels = "%Y-W%V", date_breaks = "1 month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(),
        )

ggplot(dengue_climate_rj, aes(x = data_iniSE)) +
  geom_line(aes(y = precip_total)) +
  labs(title = "", x = "Epidemiological Week", y = "Total Precipitation (mm))") +
  scale_x_date(date_labels = "%Y-W%V", date_breaks = "1 month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(),
        )

# Correlation analysis
cor_data <- dengue_climate_rj %>%
  select(tempmed, umidmed, precip_total)
cor_matrix <- cor(cor_data)
corrplot::corrplot(cor_matrix, method = "color")

## Check stationarity of the time series
acf(dengue_climate_rj$casprov, main = "ACF of Dengue Cases")
acf(dengue_climate_rj$tempmed, main = "ACF of Average Temperature")
acf(dengue_climate_rj$umidmed, main = "ACF of Average Humidity")
acf(diff(dengue_climate_rj$casprov), main = "ACF of Differenced Dengue Cases")
acf(diff(dengue_climate_rj$tempmed), main = "ACF of Differenced Average Temperature")


## Lagged correlation analysis
ccf_temp <- ccf(diff(dengue_climate_rj$casprov), diff(dengue_climate_rj$tempmed), lag.max = 10)
ccf_umid <- ccf(diff(dengue_climate_rj$casprov), dengue_climate_rj$umidmed, lag.max = 10)
ccf_precip <- ccf(diff(dengue_climate_rj$casprov), diff(dengue_climate_rj$precip_total), lag.max = 50)
