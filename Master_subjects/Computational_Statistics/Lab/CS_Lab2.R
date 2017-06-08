### Assignment 2 

## Exercise 1

data<- read.csv2("C:/Users/M/Desktop/Statistics and Data Mining Master/Semester 2/Computationa Statistics/Lecture 2/mortality_rate.csv", sep = ";", stringsAsFactors = FALSE)

# Adding loragithmic of rate

lograte<- log(data[,2])
#binding data
data<- cbind( data, lograte)


# Splitting data
n=dim(data)[1]
set.seed(123456)
id=sample(1:n, floor (n*0.5))
train=data[id,]
test=data[-id,]

mylist<- list(X = train[,1], Y = train[,3], Xtest= test[,1], Ytest= test[,3])

myseq<- seq(0.1,40,0.1)
##1.3 doing the function
counter<- 0
myMSE <- function(lambda= myseq, pars = mylist){
  mytrain<- data.frame(X =pars[["X"]],Y = pars[["Y"]])
  
  
  pred_MSE<- integer(length(lambda)) ##variable to store values
  counter <<- counter+1
  print(counter)
  for(count in 1:length(lambda)){
    model <- loess(formula = Y ~ X, data = mytrain, enp.target = lambda[count])
    fYpred<- predict(model, pars[["Xtest"]])
    pred_MSE[count]<- sum((pars[["Ytest"]]-fYpred)**2)/length(fYpred)
    
    }
  return(pred_MSE)
}

MSEresults<-myMSE(pars = mylist, lambda= myseq)

##best lambda
best<- c(best_lambda = myseq[which.min(MSEresults)], MSE = min(MSEresults))


## 1.4 plot
myplot<-function(vector1, vector2){
  library(ggplot2)
  ggplot2::ggplot(data = data.frame(X1 =vector1, X2= vector2))+
  geom_point(aes(x = X1, y= X2))+
  xlab("lambda 0.1-40 by 0.1")+ ylab("MSE")+
  theme_dark()}
myplot(myseq, MSEresults)


## 1.5


myoptimize<- optimize(myMSE , interval = c(0.1,40), tol = 0.01, pars = mylist) ##iterations = 18

## 1.6

myoptim <- optim(35 , myMSE , method = "BFGS", pars = mylist) #iterations = 3



#####2

load("C:/Users/M/Desktop/Statistics and Data Mining Master/Semester 2/Computationa Statistics/Lecture 2/data.RData")

newdata<- data

##2.2

Nloglikelihood<- function(x){
  n<- length(x)
  mu<- sum(x)/n
  var<- (1/n)*sum((x-mu)**2)
  sd<- sqrt(var)
  loglikelihood<- n*log(1/(sqrt(2*var*pi))) - sum((x-mu)**2)/(2*var)
  
  res<- list(mu = mu, var = var, sd = sd, loglikelihood= loglikelihood)
  
  return(res)
  
}

Nloglikelihood(newdata)

##2.3
count<- 0
optimloglikelihood<- function(par, x){
  
  n<- length(newdata)
  loglikelihood<- -(-n/2*log(2*pi)-n/2*log(par[2]**2) - sum((x-par[1])**2)/(2*par[2]**2))
  count<<- count+1
  print(count)
  return(loglikelihood)
  
}


###97 iterations
BFGS_no_grad<-optim(par = c(0, 1), optimloglikelihood, x= newdata , method = "BFGS")
##350 iterations
CG_no_grad<-optim(par = c(0, 1), optimloglikelihood, x= newdata , method = "CG")


##gradient
count<-0
mycount<- 0
gradient <- function(par, x){
  n<- length(newdata)
  gradient_mu<- -((1/(par[2]**2))*(sum(x)-n*par[1]))
  gradient_sigma <- -(-n/par[2] + (1/par[2]**3)*sum((x-par[1])**2))
  
  result <- c(gradient_mu = gradient_mu, gradient_sigma= gradient_sigma ) 
  mycount<<- mycount+1
  print(mycount)
  return(result)
}

##38 iterations, 15 times the gradient function
BFGS_grad <-optim(par = c(0, 1), optimloglikelihood, x= newdata, method = "BFGS", gr = gradient)
##53 iterations, 17 times the gradient function
CG_grad   <-optim(par = c(0, 1), optimloglikelihood ,x= newdata, method = "CG", gr = gradient)

##Creating a data frame for answers
myframe <- data.frame(unlist(CG_grad), unlist(CG_no_grad), unlist(BFGS_grad), unlist(BFGS_no_grad))
rownames(myframe)<- c("mu", "sigma", "loglikelihood", "counts.function", "counts.gradient", "convergence")
colnames(myframe)<- c("CG with gradient", "CG without gradient", "BFGS with gradient", "BFGS without gradient")


myframe <- rbind(myframe)
myframe
