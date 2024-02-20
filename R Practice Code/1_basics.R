#looping in R
#A for loop is used to iterate a vector

# Create fruit vector  
fruit <- c('Apple', 'Orange',"Guava", 'Pinapple', 'Banana','Grapes')  
# Create the for statement  
for ( i in fruit){   
  print(i)  
}  

#print 1-10 number
n <- c(1,2,3,4,5,6,7,8,9,10)  
for ( i in n){   
  print(i)  
}  

#count the even numbers
x <- c(2,5,3,9,8,11,6,44,43,47,67,95,33,65,12,45,12)  
count <- 0  
for (val in x) {  
  if(val %% 2 == 0)  count = count+1  
}  
print(count) 


#use of repeat loop
sum <- 0  
{  
  n1<-readline(prompt="Enter any integer value below 20: " )  
  n1<-as.integer(n1)  
}  
repeat{  
  sum<-sum+n1  
  n1=n1+1  
  if(n1>20){  
    break  
  }  
}  
cat("The sum of numbers from the repeat loop is: ",sum)  


#while loop example
m=0
while(m<5){
  print(m)
  m=m+1
}


#how to create functions
#A set of statements which are organized together to perform a specific task


# Creating a function without an argument.  
new.function <- function() {  
  for(i in 1:5) {  
    print(i^2)  
  }  
}     
#calling functions
new.function()  


# Creating a function to print squares of numbers in sequence.  
new.function <- function(a){  
  for(i in 1:a) {  
    b <- i^2  
    print(b)  
  }  
}
  # Calling the function new.function supplying 10 as an argument.  
  new.function(10)  
  
  # Creating a function with default arguments.  
  new.multi1 <- function(x = 11, y = 24) {  
    result <- x * y  
    print(result)  
  }  
  
  # Calling the function without giving any argument.  
  new.multi1()  
  
  # Calling the function with giving new values of the argument.  
  new.multi1(4,6)  

#calling in-builts function
  
  # Creating sequence of numbers from 32 to 46.  
  print(seq(32,46))  
  
  # Finding the mean of numbers from 22 to 80.  
  print(mean(22:80))  
  
  # Finding the sum of numbers from 41 to 70.  
  print(sum(41:70))  
