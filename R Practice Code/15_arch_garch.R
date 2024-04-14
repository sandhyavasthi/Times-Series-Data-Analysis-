#GARCH analysis of DJIA returns
#R fGarch package to fit an AR(1)-GARCH(1,1)
#DJIA  The daily returns of the Dow Jones Industrial Average (DJIA) from April 20, 2006 to
#April 20, 2016.

library(astsa)
library(TTR)
library(xts)   # needed to handle djia
library(fGarch)

#ACF plot for DJIA time series data
djiar = diff(log(djia$Close))[-1]
acf2(djiar)    # exhibits some autocorrelation (not shown)
acf2(djiar^2)  # oozes autocorrelation (not shown)


# GARCH fit
summary(djia.g <- garchFit(~arma(1,0)+garch(1,1), data=djiar, cond.dist='std'))
plot(djia.g)    # to see all plot options


# APARCH fit
summary(djia.ap <- garchFit(~arma(1,0)+aparch(1,1), data=djiar, cond.dist='std'))
plot(djia.ap)