
library(utils)
library(readr)
#Importing data to R from a CSV and TXT files
data1 <- read_csv('C:/Users/Sandhya Avasthi/Documents/BSU/MSDS/R programming tutorial/hotel_bookings.csv',show_col_types = FALSE)
head(data1, 5)
#to view data in a different tab
View(data1)

#read.table() can be used to load data in a table
data2 <- read.table('C:/Users/Sandhya Avasthi/Documents/BSU/MSDS/R programming tutorial/hotel_bookings.csv', sep=",", header = 1)
head(data2, 5)

#Importing data from Excel into R
library(readxl)
data4 <- read_excel("C:/Users/Sandhya Avasthi/Documents/BSU/MSDS/R programming tutorial/TeslaDeaths.xlsx", sheet = 1)
head(data4, 5)

#exploring data1
nrow(data1)
ncolumn(data1)
str(data1)

#how to access key values from dataframe, name of variable/column not counted
data1[2,3]

#accessing part of table
data1[1:3,3:5]

#specific rows , all columns
data1[1:3,]

#allrows , specific columns
data1[,1:5]

#accessing a column by name/attrbiute
data1["lead_time"]

#to get a column as a vector (sequence)
data1$adults

#filter rows of dataframe
data1[data1$stays_in_week_nights>5,]


#to create your dataframe
Testdata <- data.frame (
  Training = c("Strength", "Stamina", "Other"),
  Pulse = c(100, 150, 120),
  Duration = c(60, 30, 45)
)

# Print the data frame
Testdata

#to get summary
summary(Testdata)

#use single brackets [ ], double brackets [[ ]] or $ to access columns from a data frame
Testdata[1]

Testdata[["Training"]]

Testdata$Training

#add rows using rbind() function
# Add a new row
Newdata<- rbind(Testdata, c("Strength", 110, 110))
Newdata

# Add a new column
NewcolDF <- cbind(Newdata, Steps = c(1000, 6000, 2000, 2200))
NewcolDF

# Remove the first row and column
dataremoved <- NewcolDF[-c(1), -c(1)]
dataremoved

#to check rows and columns
dim(dataremoved)

#combining dataframes
Data_Frame1 <- data.frame (
  Training = c("Strength", "Stamina", "Other"),
  Pulse = c(100, 150, 120),
  Duration = c(60, 30, 45)
)

Data_Frame2 <- data.frame (
  Training = c("Stamina", "Stamina", "Strength"),
  Pulse = c(140, 150, 160),
  Duration = c(30, 30, 20)
)

New_Data_Frame <- rbind(Data_Frame1, Data_Frame2)
New_Data_Frame

#preprocessing and cleaning data
#Data cleaning and preprocessing in R involve various tasks, 
#such as handling missing values, removing duplicates, 
#transforming variables, and scaling data. 

#load salesdata in dataframe
salesdata <- read_csv('C:/Users/Sandhya Avasthi/Documents/BSU/MSDS/R programming tutorial/SalesData.csv',show_col_types = FALSE)

head(salesdata)
#to view data in a different tab
View(salesdata)

summary(salesdata)

#finding NAs
is.na(salesdata)

#total number of NAs
sum(is.na(salesdata))

#removing rows with NAs
S1<- na.omit(salesdata)
dim(S1)
View(S1)

#replace missing values with mean of each variable
salesdata$Sales[is.na(salesdata$Sales)] <- mean(salesdata$Sales, na.rm = TRUE)
salesdata$Profit[is.na(salesdata$Profit)] <- mean(salesdata$Profit, na.rm = TRUE)
salesdata$Unit.Price[is.na(salesdata$Unit.Price)] <- mean(salesdata$Unit.Price, na.rm = TRUE)

#load data again
S0 <- read_csv('C:/Users/Sandhya Avasthi/Documents/BSU/MSDS/R programming tutorial/SalesData.csv',show_col_types = FALSE)

#replace missing values for numeric data
S0$Sales[is.na(S0$Sales)] <- runif(n = sum(is.na(S0$Sales)),
                                   min = min(S0$Sales, na.rm = TRUE),
                                   max = max(S0$Sales, na.rm = TRUE))
S0$Profit[is.na(S0$Profit)] <- runif(n = sum(is.na(S0$Profit)),
                                     min = min(S0$Profit, na.rm = TRUE),
                                     max = max(S0$Profit, na.rm = TRUE))
S0$Unit.Price[is.na(S0$Unit.Price)] <- runif(n = sum(is.na(S0$Unit.Price)),
                                             min = min(S0$Unit.Price, na.rm = TRUE),
                                             max = max(S0$Unit.Price, na.rm = TRUE))

#Since categorical variables do not have min and max values, 
#we can replace the missing values for categorical variables 
#by random value from each variable.
S0$Order.Priority[is.na(S0$Order.Priority)] <- sample(levels(S0$Order.Priority),
                                                      size = sum(is.na(S0$Order.Priority)),
                                                      replace = TRUE)
S0$Ship.Mode[is.na(S0$Ship.Mode)] <- sample(levels(S0$Ship.Mode),
                                            size = sum(is.na(S0$Ship.Mode)),
                                            replace = TRUE)
S0$Customer.Name[is.na(S0$Customer.Name)] <- sample(levels(S0$Customer.Name),
                                                    size = sum(is.na(S0$Customer.Name)),
                                                    replace = TRUE)

#corelation plot
cor(S0$Shipping.Cost, S0$Order.Quantity)

#data exploration
summary(S1)

#standard deviation
sd(S1$`Order Quantity`)
sd(S1$Sales)
sd(S1$Profit)
sd(S1$`Unit Price`)
sd(S1$`Shipping Cost`)

#draw a boxplot for each variable
par(mfrow = c(1, 2))
#boxplot(S1$Order.Quantity`, main = "Order Quantity")
boxplot(S1$Profit, main = "Profit")

