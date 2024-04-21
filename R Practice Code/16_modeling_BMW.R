#Analysis of BMW stock price data
library(readxl)
#Upload data on R 
BMW<-read_excel("C:/Users/Sandhya Avasthi/Documents/BSU/MSDS/R programming tutorial/BMW.xlsx")

#set timeseries element
BMW$Years <- as.Date(BMW$Years, "%d/%m/%Y")
class(BMW$Years)
BMW.z = zoo(x=BMW$Prices, order.by=BMW$Years)

#Calculate log retruns and remove first NA value
Return.BMW<-Return.calculate(BMW.z, method = "log")[-1]

#estimate GARCH(1,1) or symmetric GARCH model using rugarch() package

#Specigy GARCH models:
spec = ugarchspec(variance.model=list(garchOrder=c(1,1)),
                  mean.model=list(armaOrder=c(0,0)),
                  distribution.model="norm",)

#fit GARCH model
garch.fit=ugarchfit(data=Return.BMW,spec=spec)

# Perform Engle and Ng sign and size bias tests
signbias(garch.fit)
