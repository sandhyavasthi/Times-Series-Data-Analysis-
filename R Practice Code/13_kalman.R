# Stock price forecasting using Kalman filter
library(stats)
library(astsa)
library(ggplot2)
set.seed(314)

# Read the data
data <- read.table("C:/Users/Sandhya Avasthi/Documents/BSU/MSDS/R programming tutorial/TGT.csv", sep=',', header=T)
measure <- data$Close

# the last 54 days are for testing
measure_test <- measure[200:253]

# The first 199 days are for training
measure <- measure[1:199]

# Fit an initial basic ARIMA model
fit3 <- arima(measure, c(3, 0, 0))

# Forecast 54 days using the Kalman filter working on the ARIMA model
kal_forecast <- KalmanForecast(54, fit3$model, update=TRUE)
kal <- kal_forecast$pred + fit3$coef[4] # Add the intercept

mean(abs(measure_test - kal)) # MAE
cor(measure_test, kal) # Correlation (square this to get R-squared)

lower <- kal - 1.96*sqrt(kal_forecast$var) # lower bound
upper <- kal + 1.96*sqrt(kal_forecast$var) # upper bound

p <- ggplot()
p <- p + geom_line(aes(x=1:199, y=measure, color="Original Data"))
p <- p + geom_line(aes(x=200:253, y=measure_test, color="Original Data TEST"))
p <- p + geom_line(aes(x=200:253, y=kal, color="Predicted using Kalman Filter"))
p <- p + ylab("Value of the Stock")
p <- p + xlab("Time in Days")
p <- p + ggtitle("Stock Price Prediction using Kalman Filter")
p <- p + geom_ribbon(aes(x=c(200:253), y = kal, ymin=lower, ymax=upper), linetype=2, alpha=0.1)
p


#Some example of Ksmooth
# generate some data
set.seed(1)
sQ  = 1; sR = 3; n = 100  
mu0 = 0; Sigma0 = 10; x0 = rnorm(1,mu0,Sigma0)
w = rnorm(n); v = rnorm(n)
x = c(x0 + sQ*w[1]);  y = c(x[1] + sR*v[1])   # initialize
for (t in 2:n){
  x[t] = x[t-1] + sQ*w[t]
  y[t] = x[t] + sR*v[t]   
}
# run and plot the smoother  
run = Ksmooth(y, A=1, mu0, Sigma0, Phi=1, sQ, sR)
tsplot(cbind(y,run$Xs), spaghetti=TRUE, type='o', col=c(4,6), pch=c(1,NA), margins=1)

# CRAN tests need extra white space :( so margins=1 above is not necessary otherwise
legend('topleft', legend=c("y(t)","Xs(t)"), lty=1, col=c(4,6), bty="n", pch=c(1,NA))




