#Building a predictive regression model
library(forecast)
library(fpp2)

#scenario-based modeling
fit.consBest <- tslm(
  Consumption ~ Income + Savings + Unemployment,
  data = uschange)

h <- 4
newdata <- data.frame(
  Income = c(1, 1, 1, 1),
  Savings = c(0.5, 0.5, 0.5, 0.5),
  Unemployment = c(0, 0, 0, 0))
fcast.up <- forecast(fit.consBest, newdata = newdata)
newdata <- data.frame(
  Income = rep(-1, h),
  Savings = rep(-0.5, h),
  Unemployment = rep(0, h))
fcast.down <- forecast(fit.consBest, newdata = newdata)

autoplot(uschange[, 1]) +
  ylab("% change in US consumption") +
  autolayer(fcast.up, PI = TRUE, series = "increase") +
  autolayer(fcast.down, PI = TRUE, series = "decrease") +
  guides(colour = guide_legend(title = "Scenario"))


#Prediction intervals if income is increased by its historical mean of  
#0.72% versus an extreme increase of 5

model_new <- tslm(Consumption ~ Income, data = uschange)
h <- 4

fcastave <- forecast(model_new,
                      newdata = data.frame(
                        Income = rep(mean(uschange[,"Income"]), h)))
fcastup <- forecast(model_new,
                     newdata = data.frame(Income = rep(5, h)))

#plot time series
autoplot(uschange[, "Consumption"]) +
  ylab("% change in US consumption") +
  autolayer(fcastave, series = "Average increase",
            PI = TRUE) +
  autolayer(fcastup, series = "Extreme increase",
            PI = TRUE) +
  guides(colour = guide_legend(title = "Scenario"))
