#understanding various data types
#A vector is a basic data structure which plays an important role in R programming.

#intializing a vector with range of values
a<-4:-10  
a  

#using sequence function
seq_vec<-seq(1,4,by=0.5)  
seq_vec  
class(seq_vec)  

#numeric vectors
d<-45.5  
num<-c(10.1, 10.2, 33.2)  
d  
num  
class(d)  
class(num)

#interger vector, A non-fraction numeric value is known as integer data. 
d<-as.integer(5)  
e<-5L  
class(d)  
class(e)  

#character vector, A character is held as a one-byte integer in memory.
d<-'sandhya avasthi'  
e<-"hello world"  
f<-65  
f<-as.character(f)  
d  
e  
f  

#using logical vector, it has only two values i.e., True or False.

d<-as.integer(5)  
e<-as.integer(6)  
f<-as.integer(7)  
g<-d>e  
h<-e<f  
g  
h  
log_vec<-c(d<e, d<f, e<d,e<f,f<d,f<e)  
log_vec  
class(g)  
class(h)  
class(log_vec)  
