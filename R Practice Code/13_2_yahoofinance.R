library(quantmod)
library(tidyquant)

# Example: Get GBP/USD exchange rate data from Yahoo Finance
getSymbols("GBPUSD=X", src = "yahoo", from = "2016-01-01", to = "2016-12-31")

# Check the first few rows of the data
head(`GBPUSD=X`)

# Example: Get GBP/USD exchange rate data from Yahoo Finance, this another way
library(tidyquant)
gbpusd <- tq_get("GBPUSD=X", from = "2016-01-01", to = "2016-12-31")

# Check the first few rows of the data
head(gbpusd)

plot(gbpusd$date, gbpusd$close, type = "l", col = "blue", 
     xlab = "Date", ylab = "GBP/USD Exchange Rate", main = "GBP/USD Exchange Rate in 2016")

