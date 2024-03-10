library(astsa)
library(fpp)
library(seasonal)
library(fpp2)

help(fpp)

#smoothing Oil data and graph showing trend, avaiable with fpp library
oildata <- window(oil, start=1996)
autoplot(oildata) +
  ylab("Oil (millions of tonnes)") + xlab("Year")

# applying simple exponetial smoothing
# Estimate parameters
fc <- ses(oildata, h=5)

# Accuracy of one-step-ahead training errors
round(accuracy(fc),2)

#plot showing, blackline showing changing level
autoplot(fc) +
  autolayer(fitted(fc), series="Fitted") +
  ylab("Oil (millions of tonnes)") + xlab("Year")

#example of airpassengers
air <- window(ausair, start=1990)
fc <- holt(air, h=5)

#shows the forecasts for years 2017–2031 generated 
#from Holt’s linear trend method and the damped trend method.
fc2 <- holt(air, damped=TRUE, phi = 0.9, h=15)
autoplot(air) +
  autolayer(fc, series="Holt's method", PI=FALSE) +
  autolayer(fc2, series="Damped Holt's method", PI=FALSE) +
  ggtitle("Forecasts from Holt's method") + xlab("Year") +
  ylab("Air passengers in Australia (millions)") +
  guides(colour=guide_legend(title="Forecast"))


#sheep in Asia data
#forecasting the sheep livestock population in Asia
autoplot(livestock) +
xlab("Year") + ylab("Livestock, sheep in Asia (millions)")

#comparing simple smoothing with Holt method on parameter MSE, MAE
e1 <- tsCV(livestock, ses, h=1)
e2 <- tsCV(livestock, holt, h=1)

# Compare MSE:
mean(e1^2, na.rm=TRUE)
mean(e2^2, na.rm=TRUE)

# Compare MAE:
mean(abs(e1), na.rm=TRUE)
mean(abs(e2), na.rm=TRUE)

#plotting JJ dataset
plot(jj, type="o", ylab="Quarterly Earnings per Share")
plot(globtemp, type="o", ylab="Global Temperature Deviations")


par(mfrow=c(2,1))
plot(diff(globtemp), type="o")
mean(diff(globtemp)) # drift estimate = .008
acf(diff(gtemp), 48)


par(mfrow=c(2,1))
plot(varve, main="varve", ylab="")
plot(log(varve), main="log(varve)", ylab="" )