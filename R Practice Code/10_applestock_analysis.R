library(rugarch)
library(tseries)
library(fBasics)
library(zoo) #Convert to time series data type 
library(lmtest) 
library(forecast)


#------2	Plot Original Time Series Data of Apple Stock Prices Year 2002-2015
#Load libraries and data
apple <- read.table("C:/Users/Sandhya Avasthi/Documents/BSU/MSDS/R programming tutorial/AAPL.csv",header=T, sep=',') 
#creator for ordered observations which includes irregular time series.

#see top 5 rows
head(apple)

#see bottom 5 rows
tail(apple)

applets <- zoo(apple$Adj.Close, as.Date(as.character(apple$Date), format = c("%Y-%m-%d")))


#Time series plot (non-stationary with varying mean and variance)
plot(applets, type='l', ylab = " adj close price", main="Plot of 2002-2017 daily apple stock prices")
acf(coredata(applets), main="ACF plot of the 2002-2017 daily apple stock prices")
pacf(coredata(applets), main="PACF plot of the 2002-2017 daily apple stock prices")

#------3	Log and One-Time Differencing Transformation
#To attain stationarity: log return time series
apple_rets <- log(applets/lag(applets,-1)) 
plot(apple_rets, type='l', ylab = " adj close price", main="Log return plot of 2002-2017 daily apple stock prices")

#------4	Augmented Dickey Fuller (ADF) Test
adf.test(applets) #Original 
adf.test(apple_rets) #Log-return
#strip off the dates and create numeric object
apple_ret_num <- coredata(apple_rets)

#------5	Autoregressive Conditional Heteroskedasticity (ARCH) Test 
library(zoo)
library(FinTS)
ArchTest(apple_ret_num,lag=12)

#------6	Exploratory Analysis 
#Compute statistics
basicStats(apple_rets) #mean is 0 and the distribution of log returns has large heavy tail.


#QQ-plot
qqnorm(apple_rets)
qqline(apple_rets, col = 2) 
kurtosis(apple_rets) #positive, heavy-tailed distribution


#Time plot of square of log return of prices
# mean is constant and nearly 0
plot(apple_rets^2,type='l', ylab = "square of stock price return", main="Plot of 2002-2017 daily apple stock price squared return")
#Time plot of absolute value of log return of prices
plot(abs(apple_rets),type='l', ylab = "abs value of stock price return", main="Plot of 2002-2017 daily apple stock price abs return")
par(mfrow=c(3,1)) #show three plots in a figure 
acf(apple_ret_num) #non-linear dependence
acf(apple_ret_num^2) #strong non-linear dependence
acf(abs(apple_ret_num)) #strong non-linear dependence
dev.off() 

eacf(apple_ret_num) #
eacf(abs(apple_ret_num)) #suggest garch 11

#garch 11
g11=garch(apple_ret_num,order=c(1,1))
g11
summary(g11) #checking p value 
AIC(g11)
# plot(residuals(g11),type='h',ylab='Standardized Residuals') 
# qqnorm(residuals(g11)) ;qqline(residuals(g11))
# kurtosis(residuals(g11))
# acf(residuals(g11)^2, na.action = na.omit) 
# acf(residuals(g11), na.action = na.omit)
gBox(g11,method='squared')

#garch 22
g22=garch(apple_ret_num,order=c(2,2))
summary(g22)
AIC(g22)
gBox(g22,method='squared') #not good model conduct diagonise checking 

#------7	Further Models Identification 

#---1. GARCH(1,1) with normally distributed errors
garch11.spec=ugarchspec(variance.model=list(garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)))
#estimate model 
garch11.fit=ugarchfit(spec=garch11.spec, data=apple_rets)
garch11.fit

#---2. GARCH(1,1) model with t-distribution
garch11.t.spec=ugarchspec(variance.model=list(garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)), distribution.model = "std")
#estimate model 
garch11.t.fit=ugarchfit(spec=garch11.t.spec, data=apple_rets)
garch11.t.fit

#---3. GARCH(1,1) model with skewed t-distribution
garch11.skt.spec=ugarchspec(variance.model=list(garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)), distribution.model = "sstd")
#estimate model 
garch11.skt.fit=ugarchfit(spec=garch11.skt.spec, data=apple_rets)
garch11.skt.fit

#---4. eGARCH(1,1) model with t-distribution
egarch11.t.spec=ugarchspec(variance.model=list(model = "eGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)), distribution.model = "std")
#estimate model 
egarch11.t.fit=ugarchfit(spec=egarch11.t.spec, data=apple_rets)
egarch11.t.fit

#---5. fGARCH(1,1) model with t-distribution
fgarch11.t.spec=ugarchspec(variance.model=list(model = "fGARCH", garchOrder=c(1,1), submodel = "APARCH"), mean.model=list(armaOrder=c(0,0)), distribution.model = "std")
#estimate model 
fgarch11.t.fit=ugarchfit(spec=fgarch11.t.spec, data=apple_rets)
fgarch11.t.fit

#---6. iGARCH (1,1) Model with Normal Distribution
igarch11.t.spec=ugarchspec(variance.model=list(model = "iGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(0 , 0 )), distribution.model = "std")
igarch11.t.fit=ugarchfit(spec=igarch11.t.spec, data=apple_rets)
igarch11.t.fit

#------8	Forecasting 
library(rugarch) 
#Fit ARMA(0,0)-eGARCH(1,1) model with t-distribution
egarch11.t.spec=ugarchspec(variance.model=list(model = "eGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)), distribution.model = "std")
#estimate model 
egarch11.t.fit=ugarchfit(spec=egarch11.t.spec, data=apple_rets)
egarch11.t.fit


rff=ugarchfit(spec=egarch11.t.spec, data=apple_rets, out.sample=500)
rf=ugarchforecast(rff, n.ahead=20, n.roll=450)
plot(rf, which="all")
