#import library of datasets
library(datasets)

# this line will download forecast package in your IDE
#install.packages('forecast')

library('forecast') 
class(AirPassengers)
AirPassengers

# Create a color palette for the box plot
my_colors <- rainbow(12)

# Box plot by month with customizations
boxplot(split(AirPassengers, cycle(AirPassengers)),
        xlab = "Month", ylab = "Number of Passengers",
        col = my_colors,  # Assign colors to each box
        border = "black",  # Set the border color
        main = "Monthly Air Passenger Counts by Month",
        names = month.abb,  # Use abbreviated month names as labels
        outline = FALSE)  # Remove outliers

#plot
plot(AirPassengers)

#modeling
model<-auto.arima(AirPassengers)
summary(model)

# h = 10*12 because, forecast is for 10 years for all 12 months
f<-forecast(model, level=c(95), h=10*12)
plot(f)


#case 2
#a home improvement company,DIY need demand forecast for next forecasting period
#using ARIMA future demand predicted

#Import data, create time series objects.
#    Analyse trend, seasonality.
#    Analyse trend, seasonality - Practice Task
#    Auto ARIMA modelling & parameters
#    Create and evaluate ARIMA demand model
#    Forecast demand, plot, extact and evaluate.
#    Capstone Task

#Task 1 - Import data, create time series objects
library(forecast)
library(data.table)
library(ggplot2)
# Import Demand Data
dt = fread("C:/Users/Sandhya Avasthi/Documents/BSU/MSDS/R programming tutorial/Demand-Data.csv")
View(dt)

# Format Date, add additional columns
dt$Date = as.Date(dt$Date, "yyyy-mm-dd")
dt[, WeekDay := wday(Date) ]
dt[, WeekNum := week(Date) ]
dt[, Month   := month(Date)]
dt[, Year    := year(Date) ]

# Plot demand data for a chosen product
ggplot(data = dt, aes(x = Date, y = Paint)) +
  geom_line(color = "blue")+
  xlab("Year") +
  ylab("Daily Demand for Paint")

# Summarise demand by week and by month
cnames = names(dt)[2:7]

dtw = dt[, lapply(.SD, sum, na.rm=TRUE), 
         by = .(WeekNum,Year), .SDcols=cnames ]

dtm = dt[, lapply(.SD, sum, na.rm=TRUE), 
         by = .(Month,Year), .SDcols=cnames ]

# Filtering Data Tables
dtw[Year == 2020]
dtm[Year >= 2019]

# Extracting Columns as Vectors
dtm[, Paint]
dtm[Year >= 2020, Paint]

#------Time Series Objects (Examples) ---------------

x = sample(10:20,100, replace = TRUE)

ts(x, frequency = 12, start = c(2014,2))

ts(x, frequency = 4, start = c(2014,3))

ts(x, frequency = 4, 
   start = c(2014,3), end = c(2020,4))


#-----Create Time Series Objects From Demand Data ----

ts_dtm = ts(dtm[,Paint], frequency =12, start = c(2018,1))

autoplot(ts_dtm) + 
  xlab("Year") +
  ylab("Monthly Demand for Paint")

ts_dtw = ts(dtw[,GardEquip], frequency = 52, start = c(2018,1))

autoplot(ts_dtw) + 
  xlab("Year") +
  ylab("Weekly Demand for Garden Equipment")

# Check If It's Time Series Object
is.ts(dt)  
is.ts(ts_dtw)


# ---------------------------------------------------------
# Task 2 - Analyse demand trend & seasonality 
# ---------------------------------------------------------

ts_dtw = ts(dtw[,Compost], frequency =52, start = c(2018,1))
autoplot(ts_dtw)

ts_dtm = ts(dtm[,DoorLock], frequency =12, start = c(2018,1))
autoplot(ts_dtm)

# ------ACF and PACF --------------------------------------
#   
#   ACF  = Auto-correlation Function 
#   PACF = Partial Auto-correlation Function 

Acf(ts_dtm)
Pacf(ts_dtm)


# ------Decompose Data ------------------------------------

ts_dtm  %>%
  decompose() %>%
  autoplot()

ts_dtw  %>%
  decompose() %>%
  autoplot()


# ------Extract Decomposed Data ---------------------------

decompose(ts_dtm)$trend
decompose(ts_dtm)$seasonal
decompose(ts_dtm)$random

autoplot(ts_dtm - decompose(ts_dtm)$seasonal)
autoplot(ts_dtm - decompose(ts_dtm)$trend)

Pacf(ts_dtm - decompose(ts_dtm)$seasonal)


# ---------------------------------------------------------
# Practice - Analyse demand trend & seasonality 
# ---------------------------------------------------------

# Analyse QUARTERLY trend and seasonality for "GardEquip"
# Use demand from 2019 onwards, exclude 2018 data
# You will need a new table, "dtq" and new time series object
# DO NOT use "ts_dtw" and "ts_dtm" to define time series objects
# Use the function "quarter" to create additional column in dt



# ---------------------------------------------------------
# Task 3 - Auto ARIMA modelling & parameters
# ---------------------------------------------------------

# ------What is ARIMA ? -----------------------------------
# AR     = Auto Regression - Regression on past values
# MA     = Moving Average model - Regression on past "errors"
# ARIMA  = Auto Regressive Integrated Moving Average

# AR - regression model - current values are 
#      linearly dependent on past values

# MR - regression model - current values are 
#      linearly dependent on past "errors" 

# Resource - https://otexts.com/fpp2/seasonal-arima.html


auto.arima(ts_dtm)

# ARIMA (p,d,q) (P,D,Q) [freq]
# (p,d,q) - non-seasonal part
# (P,D,Q) - seasonal part

# p,P - order (number of time lags) of the autoregressive model
# d,P - degree of first differencing 
# q,P - order of the moving-average model

# AIC = Akaike Information Criterion
# BIC = Bayesian Information Criterion

# AIC and BIC estimate prediction error
#     and quantity the information loss
#     the lesser the better model we have

# List the models using 'trace = TRUE'
auto.arima(ts_dtm, ic = "bic", trace = TRUE)

# "stepwise = FALSE" searches more models. It's slow.
auto.arima(ts_dtm, stepwise = FALSE, trace = TRUE)

# The model can be stored in a variable
m = auto.arima(ts_dtm, ic = "bic", trace = TRUE)
print(m)

# ---------------------------------------------------------
# Task 4 - Create and evaluate ARIMA demand model
# ---------------------------------------------------------

ts_dtm = ts(dtm[,Paint], frequency =12, start = c(2018,1))
autoplot(ts_dtm)

m = auto.arima(ts_dtm, ic = "bic", trace = TRUE)
print(m)

auto.arima(ts_dtm, stepwise = FALSE, trace = TRUE)

m = Arima(ts_dtm, order = c(0,0,0),
          seasonal=list(order=c(0,1,1), period=12),
          include.drift = TRUE)
print(m)

Pacf(ts_dtm)

summary(m)

checkresiduals(m) # residuals normal, ACF matches periodicity


# ---------------------------------------------------------
# Task 5 - Forecast demand, plot, extact and evaluate.
# ---------------------------------------------------------

f = forecast(m, h = 12) # h = number of periods forecasted 
autoplot(f)

autoplot(f, include = 24) # previous data points to include

#----- Forecast Diagnosis  --------------------------------

plot(f$residuals)

qqnorm(f$residuals)
qqline(f$residuals, col = 2)

Acf(f$residuals)
Pacf(f$residuals)

#----- Write Output  -------------------------------------

out = as.data.table(f)
out = round(out,0)       # round the values to integers
write.csv(out,"Forecast.csv")




# Plot the traffic on the Hyndsight blog
#Daily pageviews for the Hyndsight blog. 30 April 2014 to 29 April 2015.
#Hyndsight is Rob Hyndman's personal blog at https://robjhyndman.com/hyndsight/.
library(fpp2)
autoplot(hyndsight)

# plot ACF
ggAcf(hyndsight)

# Save the lag corresponding to maximum autocorrelation
maxlag_hyndsight <- 7

#using ARIMA model, fit is auto model
fit <- auto.arima(hyndsight, seasonal=FALSE)
fit

#forecast using ARIMA
fit %>% forecast(h=10) %>% autoplot(include=80)

#2nd model
# for ARIMA(3,0,0) as per ACF and PACF plot
fit2 <- Arima(hyndsight, order=c(3,0,0))
fit2

#forecast using ARIMA model fit2
fit2 %>% forecast(h=10) %>% autoplot(include=80)
