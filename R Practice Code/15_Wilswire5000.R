
#import data on the Wilshire 5000 index
W5000 <- read.csv("C:/Users/Sandhya Avasthi/Documents/BSU/MSDS/R programming tutorial/XX_ARCX_W5000.csv", header = TRUE)
View(W5000)

#explore data
summary(W5000)

head(W5000)  # View the first few rows of your data
names(W5000) # List column names to identify the relevant columns
dim(W5000)

# Assuming your dataset has columns 'Date' and 'Close'
W5000$Date <- as.Date(W5000$Date, format = "%m/%d/%Y")  # Convert the Date column to Date format

View(W5000)
# Check for non-numeric values
non_numeric <- W5000$Close[!grepl("^-?\\d*(\\.\\d+)?$", W5000$Close)]
print(non_numeric) # Inspect problematic values

W5000$Close <- gsub("[^0-9.]", "", W5000$Close) # Remove non-numeric characters
W5000$Close <- as.numeric(W5000$Close)         # Convert to numeric

#convert close column to numeric
W5000$Close <- as.numeric(W5000$Close)

which(is.na(W5000$Close))  # Identify rows with missing values

#clean data
W5000_clean <- W5000[!is.na(W5000$Close) & is.finite(W5000$Close), ]

#plotting the clean data
plot(W5000$Date, W5000$Close, type = "l", col = "blue", lwd = 2,
     xlab = "Date", ylab = "Closing Price",
     main = "Time Series of Closing Prices")

#to apply garch the steps are
#Install and load the rugarch package.
#Load your time series data.
#Specify the GARCH model.
#Fit the model to your data.
#Analyze the results and optionally make predictions.



#explore data of around 2 year 
W2Y<- read.csv("C:/Users/Sandhya Avasthi/Documents/BSU/MSDS/R programming tutorial/XX_ARCX_W50002Y.csv", header = TRUE)

head(W2Y)  # View the first few rows of your data
names(W2Y) # List column names to identify the relevant columns
dim(W2Y)

# Assuming your dataset has columns 'Date' and 'Close'
W2Y$Date <- as.Date(W2Y$Date, format = "%m/%d/%Y")  # Convert the Date column to Date format

#View(W2Y)
# Check for non-numeric values
non_numeric <- W2Y$Close[!grepl("^-?\\d*(\\.\\d+)?$", W2Y$Close)]
print(non_numeric) # Inspect problematic values

W2Y$Close <- gsub("[^0-9.]", "", W2Y$Close) # Remove non-numeric characters
W2Y$Close <- as.numeric(W2Y$Close)         # Convert to numeric

#convert close column to numeric
W2Y$Close <- as.numeric(W2Y$Close)

which(is.na(W2Y$Close))  # Identify rows with missing values
sum(is.na(W2Y$Close)) # Count NA values


#clean data
W2Y_clean <- W2Y[!is.na(W2Y$Close) & is.finite(W2Y$Close), ]

#plotting the clean data
plot(W2Y$Date, W2Y$Close, type = "l", col = "blue", lwd = 2,
     xlab = "Date", ylab = "Closing Price",
     main = "Time Series of Closing Prices")
