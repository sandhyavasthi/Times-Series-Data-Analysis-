
library(fpp)
library(fpp2)

# plot
autoplot(uschange[,c("Consumption","Income")]) +
  ylab("% change") + xlab("Year")


#scatterplot of uschange
uschange %>%
  as.data.frame() %>%
  ggplot(aes(x=Income, y=Consumption)) +
  ylab("Consumption (quarterly % change)") +
  xlab("Income (quarterly % change)") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)
#> `geom_smooth()` using formula = 'y ~ x'


#linear regresssion
#equation is estimated in R using the tslm() function
tslm(Consumption ~ Income, data=uschange)

#plotting US consumption data
autoplot(uschange[,c("Consumption","Income")]) +
  ylab("% change") + xlab("Year")

#regression modeling on US consumption data
fit.consMR <- tslm(
  Consumption ~ Income + Production + Unemployment + Savings,
  data=uschange)
summary(fit.consMR)

#plots show the actual values compared to the fitted values for 
#the percentage change in the US consumption expenditure series.
autoplot(uschange[,'Consumption'], series="Data") +
  autolayer(fitted(fit.consMR), series="Fitted") +
  xlab("Year") + ylab("") +
  ggtitle("Percent change in US consumption expenditure") +
  guides(colour=guide_legend(title=" "))

#verified by the strong positive relationship shown by the scatterplot
cbind(Data = uschange[,"Consumption"],
      Fitted = fitted(fit.consMR)) %>%
  as.data.frame() %>%
  ggplot(aes(x=Data, y=Fitted)) +
  geom_point() +
  ylab("Fitted (predicted values)") +
  xlab("Data (actual values)") +
  ggtitle("Percent change in US consumption expenditure") +
  geom_abline(intercept=0, slope=1)

#plot residuals,  time plot, the ACF and the histogram 
checkresiduals(fit.consMR)

#plot residual against fitted values
cbind(Fitted = fitted(fit.consMR),
      Residuals=residuals(fit.consMR)) %>%
  as.data.frame() %>%
  ggplot(aes(x=Fitted, y=Residuals)) + geom_point()
