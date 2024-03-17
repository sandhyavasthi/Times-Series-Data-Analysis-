
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


#forecasting
fit3 %>% forecast(h=12) %>% autoplot()



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
fit1

#ARIMA(3,0,1)(0,1,1)12
fit2 <- Arima(h02, order=c(3,0,1), seasonal=c(0,1,1),
              lambda=0)
fit2

#ARIMA(3,0,1)(2,1,0)12
fit3 <- Arima(h02, order=c(3,0,1), seasonal=c(2,1,0),
              lambda=0)
fit3


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
#plot European quarterly retail trade

ggplot(data = driversdata, aes(x = time, y = value))+
  geom_line(color = "#00AFBB", size = 0.5) + ggtitle('Drivers Data')+
  xlab('time') + ylab('Driver Fatality ')

