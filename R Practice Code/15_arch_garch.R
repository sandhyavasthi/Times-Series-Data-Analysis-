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


#using quantmod library for stock data
library(quantmod)
stock_namelist <- c("SPY", "TLT", "IEF")

# download data from YahooFinance
prices <- xts()
for (stock_index in 1:length(stock_namelist))
  prices <- cbind(prices, Ad(getSymbols(stock_namelist[stock_index], 
                                        from = "2013-01-01", to = "2016-12-31", auto.assign = FALSE)))
colnames(prices) <- stock_namelist
indexClass(prices) <- "Date"
logreturns <- diff(log(prices))[-1]
head(prices)


# plot the three series of log-prices
plot(log(prices), col = c("black", "blue", "magenta"),
     main = "Log-prices of the three ETFs", legend.loc = "topleft")



#applying garch on WIPRO data
library(ggplot2)
library(zoo)
library(quantmod)
library(xts)
library(PerformanceAnalytics)
library(rugarch)

#WIPRO as our stock for forecasting with the last 10 years of stock data.
getSymbols("WIPRO.NS",from = "2011-01-01",to = "2021-03-31")

#show time series plot
chartSeries(WIPRO.NS)

lreturn <- WIPRO.NS$WIPRO.NS.Close

#Wipro Daily Closing prices
# remove the null values from our dataset abd then convert it into returns data
#Then have a look at this time series
lreturn<- na.omit(lreturn)
return <- CalculateReturns(WIPRO.NS$WIPRO.NS.Close)
return <- return[-1]
return<- na.omit(return)

#look at our data in the form of chart series to get a better understanding
autoplot(return)

#to see histogram
hist(return)

chart.Histogram(return,
                methods = c('add.density', 'add.normal'),
                colorset = c('blue', 'green', 'red'))

chartSeries(return)

#look at the annual volatility of Wipro stock using Rolling statistics.
chart.RollingPerformance(R = return["2011::2020"],
                         width = 22,
                         FUN = "sd.annualized",
                         scale = 252,
                         main = "Wipro's yearly rolling volatility")

#Before applying GARCH model, we need 
#to find the right set of values of GARCH order and ARMA order
#build a set of models using standard GARCH by tweaking the order values a bit, and 
#trying different combinations to find the most suitable one for forecasting.

#model will be chosen by looking for 
#the least AIC(Akaike Information Criterion) value in each model.

#GARCH(1,1) and ARMA(0,0)
s1 <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),
                 mean.model=list(armaOrder=c(0,0)),distribution.model="norm")
m1 <- ugarchfit(data = return, spec = s1)
m1


#plot values
plot(m1, which = 'all')

#GARCH(1,1) and ARMA(2,2)
s2 <-  ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),
                  mean.model=list(armaOrder=c(2,2)),distribution.model="norm")
m2 <- ugarchfit(data = return, spec = s2)
m2

#plot m2
plot(m2, which = 'all')

#Now here with Garch order (1,2) and ARMA values (2,2)
s3 <-  ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,2)),
                  mean.model=list(armaOrder=c(2,2)),distribution.model="norm")
m3 <- ugarchfit(data = return, spec = s3)
m3

#plot m3
plot(m3, which = 'all')

#here with Garch order (2,1) and ARMA values (1,1)
s4 <-  ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(2,1)),
                  mean.model=list(armaOrder=c(1,1)),distribution.model="norm")
m4 <- ugarchfit(data = return, spec = s4)
m4


#We can see that at GARCH order(1,2) and ARMA order (2,2) has the least AIC value, 
#so we will move further with this.

#Use GARCH(1,2) and ARMA(2,2) for next 30 days forecasting
s2final <-  ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,2)),
                       mean.model=list(armaOrder=c(2,2)),distribution.model="norm")
m2final <- ugarchfit(data = return, spec = s2final)
f <- ugarchforecast(fitORspec = m2final, n.ahead = 30)
plot(fitted(f))

# plot
plot(sigma(f))

#Prediction
#Forecasting our predictions on actual stock price values using our model

sfinal <- s2
setfixed(sfinal) <- as.list(coef(m2))



sim <- ugarchpath(spec = sfinal,
                  m.sim = 1,
                  n.sim = 1*30,
                  rseed = 16)
plot.zoo(fitted(sim))

#predicted value
prediction_April_Wipro<- 410.10*apply(fitted(sim), 2, 'cumsum') + 410.10
matplot(prediction_April_Wipro, type = "l", lwd = 3)

###We can see that our Wipro stock price is increasing over the month of april with Highest Closing price 
#of around Rs468 and Lowest Closing price of around Rs409