#importing useful libraries
library(ggplot2)
library(ggfortify)

#creating ts object
y <- ts(c(123,39,78,52,110), start=2012)
print(y)

#ts object with frequency 
z=c(10,20,30,40,50,60,70,80,88,87)
y <- ts(z, start=2003, frequency=12)
print(y)

library(fpp)
plot(melsyd)


#weekly economy passenger load on Ansett Airlines between Australia’s two largest cities.

autoplot(melsyd[,"Economy.Class"]) +
  ggtitle("Economy class passengers: Melbourne-Sydney") +
  xlab("Year") +
  ylab("Thousands")


#plot antidiabatic drug sales
autoplot(a10) +
  ggtitle("Antidiabetic drug sales") +
  ylab("$ million") +
  xlab("Year")


#seasonal plot, plotted againd individual season
ggseasonplot(a10, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("$ million") +
  ggtitle("Seasonal plot: antidiabetic drug sales")

#seasonal plot uses polar coordinates. Setting polar=TRUE makes 
#the time series axis circular rather than horizontal
ggseasonplot(a10, polar=TRUE) +
  ylab("$ million") +
  ggtitle("Polar seasonal plot: antidiabetic drug sales")


#seasonal subseries plot
# horizontal lines indicate the means for each month.
ggsubseriesplot(a10) +
  ylab("$ million") +
  ggtitle("Seasonal subseries plot: antidiabetic drug sales")

plot(elecdemand)
#two time series for demand and temperature
autoplot(elecdemand[,c("Demand","Temperature")], facets=TRUE) +
  xlab("Year: 2014") + ylab("") +
  ggtitle("Half-hourly electricity demand: Victoria, Australia")


#lag plot for ausbeer dataset
beer2 <- window(ausbeer, start=1992)
gglagplot(beer2)


#Autocorrelation function of quarterly beer production.
ggAcf(beer2)

#trend and seasonality 
aelec <- window(elec, start=1980)
autoplot(aelec) + xlab("Year") + ylab("GWh")

#ACF of monthly Australian electricity demand.
ggAcf(aelec, lag=48)


#white nnoise
set.seed(30)
y <- ts(rnorm(50))
autoplot(y) + ggtitle("White noise")

#acf
ggAcf(y)
