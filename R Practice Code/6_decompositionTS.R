

#X11 method is available using the seas() function from the seasonal package for R.
library(seasonal)
library(fpp)
library(fpp2)

#moving average smoothing
#volume of electricity sold to residential customers 
#in South Australia each year from 1989 to 2008
autoplot(elecsales) + xlab("Year") + ylab("GWh") +
  ggtitle("Annual electricity sales: South Australia")

#use m=5, 5 year period 
ma(elecsales, 5)

print(elecsales)
print(elecdaily)

View(elecdaily)
View(elecsales)



#trend-cycle estimate
autoplot(elecsales, series="Data") +
  autolayer(ma(elecsales,5), series="5-MA") +
  xlab("Year") + ylab("GWh") +
  ggtitle("Annual electricity sales: South Australia") +
  scale_colour_manual(values=c("Data"="grey50","5-MA"="red"),
                      breaks=c("Data","5-MA"))

#classical decomposition on electrical equpment index time series data
elecequip %>% decompose(type="multiplicative") %>%
  autoplot() + xlab("Year") +
  ggtitle("Classical multiplicative decomposition
    of electrical equipment index")


#applying X11 decomposiiton
elecequip %>% seas(x11="") -> fit
autoplot(fit) +
ggtitle("X11 decomposition of electrical equipment index")


#the trend-cycle component and the seasonally adjusted data, along with the original data.
autoplot(elecequip, series="Data") +
  autolayer(trendcycle(fit), series="Trend") +
  autolayer(seasadj(fit), series="Seasonally Adjusted") +
  xlab("Year") + ylab("New orders index") +
  ggtitle("Electrical equipment manufacturing (Euro area)") +
  scale_colour_manual(values=c("gray","blue","red"),
                      breaks=c("Data","Seasonally Adjusted","Trend"))

#a seasonal sub-series plot of the seasonal component
fit %>% seasonal() %>% ggsubseriesplot() + ylab("Seasonal")

#STL decomposition
elecequip %>%
  stl(t.window=13, s.window="periodic", robust=TRUE) %>%
  autoplot()

#forecasting with stl decomposition
fit <- stl(elecequip, t.window=13, s.window="periodic",
           robust=TRUE)
fit %>% seasadj() %>% naive() %>%
  autoplot() + ylab("New orders index") +
  ggtitle("Naive forecasts of seasonally adjusted data")


#resulting forecasts of the original data are shown
fit %>% forecast(method="naive") %>%
  autoplot() + ylab("New orders index")

fcast <- stlf(elecequip, method='naive')
