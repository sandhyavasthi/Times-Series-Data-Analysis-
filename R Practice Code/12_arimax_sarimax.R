library(astsa)
library(fpp)
library(seasonal)
library(fpp2)


# with extra variable 
#ARIMAX is forecasting the Quarterly changes 
#in US Consumption based on time and its personal income.
autoplot(uschange[,1:2], facets=TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("Quarterly changes in US consumption
    and personal income")

#fit model with extra variable/predictors
fit <- auto.arima(uschange[,"Consumption"],
                  xreg=uschange[,"Income"])
# check metrics
fit

#estimation using residuals functions
cbind("Regression Errors" = residuals(fit, type="regression"),
      "ARIMA errors" = residuals(fit, type="innovation")) %>%
  autoplot(facets=TRUE)

#check residuals
checkresiduals(fit)


#forecasting
#To forecast using a regression model with ARIMA errors, we need 
#to forecast the regression part of the model and the ARIMA part of the model, 
#and combine the results.

fcast <- forecast(fit, xreg=rep(mean(uschange[,2]),8))
autoplot(fcast) + xlab("Year") +
  ylab("Percentage change")
fcast

#Daily electricity demand and maximum daily temperature 
#for the state of Victoria in Australia for 2014.
#fit a quadratic regression model with ARMA errors using the auto.arima() function.
xreg <- cbind(MaxTemp = elecdaily[, "Temperature"],
              MaxTempSq = elecdaily[, "Temperature"]^2,
              Workday = elecdaily[, "WorkDay"])
fit <- auto.arima(elecdaily[, "Demand"], xreg = xreg)
checkresiduals(fit)


#forecasting
fcast <- forecast(fit,
                  xreg = cbind(MaxTemp=rep(26,14), MaxTempSq=rep(26^2,14),
                               Workday=c(0,1,0,0,1,1,1,1,1,0,0,1,1,1)))
autoplot(fcast) + ylab("Electricity demand (GW)")



#using Airpassenger data
x = AirPassengers
lx = log(x)
dlx = diff(lx)

#twelfth-order difference is applied and stored in ddlx.
ddlx = diff(dlx, 12)

#plot timeseries
plot.ts(cbind(x,lx,dlx,ddlx), main="")
# below of interest for showing seasonal RW (not shown here):
par(mfrow=c(2,1))
monthplot(dlx); monthplot(ddlx)

#plot ACF
acf2(ddlx,50)

#try SARIMA
sarima(lx, 1,1,1, 0,1,1,12)
sarima(lx, 0,1,1, 0,1,1,12)
sarima(lx, 1,1,0, 0,1,1,12)
