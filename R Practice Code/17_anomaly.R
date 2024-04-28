#using time series for amomaly detection

library(tidyverse)
library(anomalize)

#load data, monthly electricity consumption
data = read.csv("C:/Users/Sandhya Avasthi/Documents/BSU/MSDS/R programming tutorial/electricity_consumption.csv")
head(data)

#The datatype of Bill_Date is chr. We need to convert it to date. 
#it is recorded every month first day. Hence we will be converting Bill_Date column into Date type.
data$Bill_Date <- as.Date(data$Bill_Date, format = "%Y-%m-%d")
data <- as_tibble(data)
head(data)

# checking anomalies on the On_peak column
data_anomalized <- data %>%
  time_decompose(On_peak, merge = TRUE) %>%
  anomalize(remainder) %>%
  time_recompose()
# preview
head(data_anomalized)

#plot to visualize
#alpha_dots controls the transparency of the dots.
data_anomalized %>% plot_anomalies(alpha_dots = 0.75)

#To plot each of trend, seasonality and residue, along with the observed value,
plot_anomaly_decomposition(data_anomalized)

#First decompose the dataset, find anomalies using anomalize, 
#recompose it and then use the filter command
data %>% 
  time_decompose(On_peak) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  filter(anomaly == 'Yes')

#controlling number of amomalies
#Suppose we want 25% dataset to be anomaly. So we change the value to 0.25.
data %>%
  time_decompose(On_peak) %>%
  anomalize(remainder, alpha = 0.75, max_anoms = 0.25) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE) +
  ggtitle("25% Anomalies Allowed")

#Similarly if we want just 5%, we will provide 0.05 in max_anoms.
data %>%
  time_decompose(On_peak) %>%
  anomalize(remainder, alpha = 0.75, max_anoms = 0.05) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE) +
  ggtitle("5% Anomalies Allowed ")


