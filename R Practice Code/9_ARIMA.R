library(fpp)
library(seasonal)
library(fpp2)

View(uschange)
#us change time series data
autoplot(uschange[,"Consumption"]) +
  xlab("Year") + ylab("Quarterly percentage change")

#us change time series data, attribute income
autoplot(uschange[,"Income"]) +
  xlab("Year") + ylab("Quarterly percentage change")

#us change time series data, attribute income
autoplot(uschange[,"Savings"]) +
  xlab("Year") + ylab("Quarterly percentage change")


#using ARIMA model, fit is auto model
model1 <- auto.arima(uschange[,"Consumption"], seasonal=FALSE)
model1

#forecast using ARIMA
model1 %>% forecast(h=15) %>% autoplot(include=80)

# ACF plot
ggAcf(uschange[,"Consumption"])

#PACF plot
ggPacf(uschange[,"Consumption"])

# for ARIMA(3,0,0) as per ACF and PACF plot
model2 <- Arima(uschange[,"Consumption"], order=c(3,0,0))
model2

#forecast using model fit2
model2 %>% forecast(h=8) %>% autoplot(include=80)

# model fit 3 by making stepwise false and aprroximation false
model3 <- auto.arima(uschange[,"Consumption"], seasonal=FALSE,
                   stepwise=FALSE, approximation=FALSE)
model3


#seasonally adjusted ,electrical equipment orders data 
#time plot shows some sudden changes, particularly the big drop in 2008/2009
elecequip %>% stl(s.window='periodic') %>% seasadj() -> eeadj
autoplot(eeadj)

#diffrenced data
eeadj %>% diff() %>% ggtsdisplay(main="")

#fit an ARIMA(3,1,0) model along with variations including ARIMA(4,1,0), ARIMA(2,1,0), 
#ARIMA(3,1,1), etc. Of these, the ARIMA(3,1,1) has a slightly smaller AICc value.

fit <- Arima(eeadj, order=c(3,1,1))
fit 
#checking residuals
checkresiduals(fit)

#forecast fro chosen model
autoplot(forecast(fit))
