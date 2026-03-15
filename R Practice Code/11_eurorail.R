
library(fpp)
library(seasonal)
library(fpp2)
library(ggplot2)

View(euretail)
#plot European quarterly retail trade
autoplot(euretail) + ylab("Retail index") + xlab("Year")

#after diffrencing
euretail %>% diff(lag=4) %>% ggtsdisplay()

#double differenced 
euretail %>% diff(lag=4) %>% diff() %>% ggtsdisplay()

#Seasonal ARIMA
euretail %>%
  Arima(order=c(0,1,1), seasonal=c(0,1,1)) %>%
  residuals() %>% ggtsdisplay()

# model SARIMA
fit3 <- Arima(euretail, order=c(0,1,3), seasonal=c(0,1,1))
checkresiduals(fit3)
fit3

fit4 <- Arima(euretail, order=c(0,1,2), seasonal=c(0,1,1))
checkresiduals(fit4)
fit4

#forecasting
fit3 %>% forecast(h=12) %>% autoplot()

#forecasting
fit4 %>% forecast(h=12) %>% autoplot()

#Analysis of Corticosteroid drug sales in Australia
#to forecast monthly corticosteroid drug sales in Australia. 
#These are known as H02 drugs under the Anatomical Therapeutic 
#Chemical classification scheme.

lh02 <- log(h02)
cbind("H02 sales (million scripts)" = h02,
      "Log H02 sales"=lh02) %>%
  autoplot(facets=TRUE) + xlab("Year") + ylab("")

#after seasonally differenced
lh02 %>% diff(lag=12) %>%
  ggtsdisplay(xlab="Year",
              main="Seasonally differenced H02 scripts")

#possible model for these data is an ARIMA(3,0,0)(2,1,0)12
#fit this model, along with some variations on it, and 
#compute the AICc values 

fit <- Arima(h02, order=c(3,0,1), seasonal=c(0,1,2),
             lambda=0)
checkresiduals(fit, lag=36)
fit

#ARIMA(3,0,1)(1,1,1)12
fit1 <- Arima(h02, order=c(3,0,1), seasonal=c(1,1,1),
             lambda=0)
checkresiduals(fit1, lag=36)
fit1

#ARIMA(3,0,1)(0,1,1)12
fit2 <- Arima(h02, order=c(3,0,1), seasonal=c(0,1,1),
              lambda=0)
checkresiduals(fit2, lag=36)
fit2

#ARIMA(3,0,1)(2,1,0)12
fit3 <- Arima(h02, order=c(3,0,1), seasonal=c(2,1,0),
              lambda=0)
fit3

#ARIMA(3,0,1)(2,1,2)12
fit4 <- Arima(h02, order=c(3,0,1), seasonal=c(2,1,2),
              lambda=0)
checkresiduals(fit4, lag=36)
fit4

#ARIMA(4,1,1)(0,1,2)12
fit5 <- Arima(h02, order=c(4,1,1), seasonal=c(0,1,2),
              lambda=0)
checkresiduals(fit5, lag=36)
fit5

#ARIMA(4,1,1)(1,1,2)12
fit6 <- Arima(h02, order=c(4,1,1), seasonal=c(1,1,2),
              lambda=0)
checkresiduals(fit6, lag=36)
fit6

#ARIMA(4,1,1)(2,1,2)12
fit7 <- Arima(h02, order=c(4,1,1), seasonal=c(2,1,2),
              lambda=0)
checkresiduals(fit7, lag=36)
fit7

#ARIMA(5,1,1)(2,1,2)12
fit8 <- Arima(h02, order=c(5,1,1), seasonal=c(2,1,2),
              lambda=0)
checkresiduals(fit8, lag=36)
fit8

#ARIMA(4,1,1)(3,1,2)12
fit9 <- Arima(h02, order=c(4,1,1), seasonal=c(3,1,2),
              lambda=0)
checkresiduals(fit9, lag=36)
fit9

#ARIMA(3,0,3)(2,1,3)12
fit10<- Arima(h02, order=c(3,0,3), seasonal=c(2,1,3),
              lambda=0)
checkresiduals(fit10, lag=36)
fit10

#Forecasts from the ARIMA(3,0,1)(0,1,2)12
#model (which has the lowest RMSE value on the test set, and the best AICc value 
#amongst models with only seasonal differencing
h02 %>%
  Arima(order=c(3,0,1), seasonal=c(0,1,2), lambda=0) %>%
  forecast() %>%
  autoplot() +
  ylab("H02 sales (million scripts)") + xlab("Year")


#loading drivers data
#url=https://vincentarelbundock.github.io/Rdatasets/csv/MASS/drivers.csv

driversdata<- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/MASS/drivers.csv")
View(driversdata)

# Convert driversdata to a time series
drivers_ts <- ts(driversdata, frequency = 12)  

View(drivers_ts)
#plot drivers data
plot(drivers_ts)

#plot drivers data
autoplot(drivers_ts) + ylab("fatality rate") + xlab("time")

#after seasonally differenced
drivers_ts_diff <- diff(drivers_ts, lag = 12)  # Perform seasonal differencing


#plot seasonally differenced data
ggtsdisplay(drivers_ts_diff, xlab = "Year", main = "Seasonally differenced Drivers fatality")


#applying SARIMA using synthetic data
# SARIMA models seasonal patterns effectively
# Install required packages (run this once)


# Load the libraries
library(forecast)
library(ggplot2)
library(tseries)

#Create Synthetic Monthly Sales Data
# Create synthetic monthly sales data
set.seed(123)  # For reproducibility
months <- seq(as.Date("2020-01-01"), by = "month", length.out = 36)
sales <- 200 + (1:36) * 3 + 20 * sin(2 * pi * (1:36) / 12) + rnorm(36, mean = 0, sd = 10)
data <- data.frame(Date = months, Sales = sales)
head(data)

print(data$Sales)
print(data$Date)
# Convert to time series format
ts_data <- ts(data$Sales, start = c(2020, 1), frequency = 12)

# Visualize the original data with color
autoplot(ts_data, series = "Sales") + 
  ggtitle("Synthetic Monthly Sales Data") + 
  xlab("Time") + 
  ylab("Sales") +
  scale_color_manual(values = "blue") +  # Customize line color
  theme_minimal(base_size = 15) +  # Set base font size for better visibility
  theme(legend.position = "bottom")

# Check for stationarity
adf_test <- adf.test(ts_data)
print(adf_test)

# Identify model parameters with Auto ARIMA
auto_model <- auto.arima(ts_data)
summary(auto_model)

# Fit the SARIMA model
sarima_model <- Arima(ts_data, order=c(1,1,1), seasonal=c(1,1,1))
summary(sarima_model)

# Forecast the next 12 months
forecasted_values <- forecast(sarima_model, h=12)

# Plot the forecasted values
autoplot(forecasted_values) + 
  ggtitle("Sales Forecast for Next 12 Months") + 
  xlab("Time") + 
  ylab("Sales") +
  theme_minimal()

# Evaluate model performance
accuracy(forecasted_values)
