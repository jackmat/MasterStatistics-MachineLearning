###Assignment 1 

##1
x1 <-  1/3 
x2  <-  1/4
if(x1 - x2 == 1/12){
  print("Substracion is correct")
} else{ 
  print("Substraction is wrong")}


x1-x2


x1<- 1; x2 <- 1/2

if(x1-x2 == 1/2){
  print("Substracion is correct")
} else{ 
  print("Substraction is wrong")}

##Floats are rounded so usual mathematical laws do not hold floating point arithmetic


##2


der<- function(x, e= 10**-15){
  
  res<- ((x+e)-x)/e
  return(res)

  }

der(x= 1)
der(x = 100000)

##3

###myvar function
myvar<- function(x){
  
  res<- ((sum(x**2)-((sum(x)**2)*1/length(x)))/(length(x)-1))
  return(res)
}

set.seed(12345)


myvector<- rnorm(10000, 10**8 ,1)
myvar(myvector)
var(myvector)

result<- integer(0)


for(i in 2:length(myvector)){
  result[i]<- myvar(myvector[1:i])-var(myvector[1:i]) 
  
}

result[1]<-0 ##chaning NA for 0 in the first instance


library(ggplot2)

myplot<-function(vector){
  ggplot2::ggplot(data = as.data.frame(result))+
    geom_point(aes(x = 1:length(vector), y= result))+
    theme_dark()}
myplot(result)

#The var function calculates an estimator for the population variance (the sample variance)
#http://stackoverflow.com/questions/28637908/why-is-the-var-function-giving-me-a-different-answer-than-my-calculated-varian

##sample variance giving same graphic reuslt
pop_var<- function(x){
   return(sum((x-mean(x))**2)/(length(x)-1))
}

newresult<- integer(0)
for(i in 2:length(myvector)){
  newresult[i]<- pop_var(myvector[1:i])-var(myvector[1:i]) 
  
}

newresult[1]<-0 ##chaning NA for 0 in the first instance
myplot(newresult)
myplot(result)

##4


data<- read.csv("C:/Users/M/Desktop/Statistics and Data Mining Master/Semester 2/Computationa Statistics/tecator.csv", sep = ",")
formula <- as.formula(Protein~.)
X <- model.matrix( formula, data= data)
y <-data[,all.vars(formula)[1]]

A <- t(X)%*%X
b <- t(X)%*%y

Beta <- solve((t(X)%*%X))%*%t(X)%*%y

##scaled data
scaled_data<- as.data.frame(scale(data))

formula <- as.formula(Protein~.)
X <- model.matrix( formula, data= scaled_data)
y <-scaled_data[,all.vars(formula)[1]]

A <- t(X)%*%X
b <- t(X)%*%y

Beta <- solve((t(X)%*%X))%*%t(X)%*%y
