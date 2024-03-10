
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

