###lab2
##Activity1
#time = X, temp = Y
#data<- read.csv("C:/Users/M/Desktop/Statistics and Data Mining Master/Semester 2/Bayesian Learning/lab2/TempLinkoping2016.txt", sep ="\t")
#data<- read.csv("/home/carsa564/Desktop/Bayesian learning/TempLinkoping2016.txt", sep ="\t")
data<- read.csv("C:/Users/Carles/Desktop/Bayesian learning/Part2/TempLinkoping2016.txt", sep ="\t")

#1a
####Checking data
model<-lm(temp ~ time + I(time^2), data = data)
summary(model)
mypred<-predict(model, data)
plot(data)
lines(x =data[,1], y = mypred, col = "red")
###Initialize variables
mu_0<-model$coefficients
omega_0<-diag(3)
v_0<-6
sigma2_0<-16



###1.2
#install.packages("geoR")
library("geoR")
#install.packages("gridExtra")
library(gridExtra)
library(MASS)

jointPrior<- function(n, mu, omega , v, sigma2){
  invomega<- solve(omega)
  myres<- matrix(ncol = length(mu), nrow = n)
  for(i in 1:n){
    mysigma2<- rinvchisq(1,v,scale= sigma2)
    myres[i,]<-mvrnorm(1,mu, mysigma2*invomega)
     
  }
  return(myres)
}


myjointbetas<-jointPrior(n = 10000, mu = mu_0, omega= omega_0, v= v_0, sigma2=sigma2_0 )
colnames(myjointbetas)<- c("beta1", "beta2", "beta3")
mybetas<- colMeans(myjointbetas)
###Distributions of Beta
par(mfrow=c(1,3))
hist1<-hist(myjointbetas[,1], breaks = 30, xlab = "Beta1", main = "Distribution of Beta1")
hist2<-hist(myjointbetas[,2], breaks = 30, xlab = "Beta2", main = "Distribution of Beta2")
hist3<- hist(myjointbetas[,3], breaks = 30, xlab = "Beta3", main = "Distribution of Beta3")




##1.3
mydata<- model.matrix(temp ~ time + I(time^2), data= data)
mu_n<-as.vector(solve(t(mydata)%*%mydata+ diag(3))%*%(t(mydata)%*%mydata%*%mybetas+diag(3)%*%mu_0))
omega_n<-(t(mydata)%*%mydata + omega_0)
v_n<-nrow(data)+v_0
sigma2_n<-as.vector(v_0*sigma2_0+(as.vector(t(data$temp)%*%(data$temp)))+t(mu_0)%*%omega_0%*%mu_0-t(mu_n)%*%omega_n%*%mu_n)/v_n


myposteriorbetas<-jointPrior(n = 10000, mu = mu_n, omega= omega_n, v= v_n, sigma2=sigma2_n )

##Calculating all values for all betas getting a matrix of 10000x366
mydatapred<- matrix(nrow= 10000, ncol= nrow(data))

for(i in 1:10000){
  beta<- myposteriorbetas[i,]
  mydatapred[i, ]<-colSums(t(mydata)*beta)
}
dim(mydatapred)

hist(mydatapred[,1])
##Calculate values for lower 5% and higher 95% given normality on data

Colmeans<- apply(mydatapred,2, mean)
plot(y=Colmeans, x = data[,1])


mymat<-matrix(nrow=nrow(data), ncol = 2)
colnames(mymat)<- c("lowbound", "highbound")
for(i in 1:nrow(data)){
  mymat[i,]<- quantile(mydatapred[,i], probs = c(0.025, 0.975))
}


Mainlines<-as.data.frame(cbind(Colmeans, mymat))
# #install.packages("ggplot2")
# library(ggplot2)
# myframe<-t(mydatapred)
# newdata<-as.data.frame(cbind(data$time,  Mainlines, myframe ))
# ggplot(newdata)+
#   geom_line(aes(x=V1, y = Colmeans))+
#   geom_line(aes(x=V1, y = lowbound, color= "red"))+
#   geom_line(aes(x=V1, y = highbound, color= "red"))+
#   for(i in 5:1000)
# class(mydatapred)


par(mfrow= c(1,1))

plot(y=data$temp,x=data$time)
lines( y = Mainlines$highbound ,x=data$time, col= "red")
lines(y =Mainlines$lowbound,x=data$time, col = "red")
lines(y=Mainlines$Colmeans,x=data$time,col="blue")



###2

Womenwork<-read.csv("/home/carsa564/Desktop/Bayesian learning/WomenWork.dat.txt", sep = "") 




glmModel <-   glm(Work ~ 0 + ., data = Womenwork, family = binomial)




#install.packages("mvtnorm") # Loading a package that contains the multivariate normal pdf
library("mvtnorm") # This command reads the mvtnorm package into R's memory. NOW we can use dmvnorm function.

# Loading data from file
Data<-read.csv("/home/carsa564/Desktop/Bayesian learning/WomenWork.dat.txt", sep = "") 
tau <- 10;         # Prior scaling factor such that Prior Covariance = (tau^2)*I
chooseCov <- c(2:8)  # Here we choose which covariates to include in the model

y <- as.vector(Data[,1]); # Data from the read.table function is a data frame. Let's convert y and X to vector and matrix.
X <- as.matrix(Data[,2:ncol(Data)]);
covNames <- names(Data)[2:length(names(Data))];
X <- X[,chooseCov]; # Here we pick out the chosen covariates.
covNames <- covNames[chooseCov];
nPara <- dim(X)[2];

# Setting up the prior
mu <- as.vector(rep(0,nPara)) # Prior mean vector
Sigma <- tau^2*diag(nPara);


LogPostLogistic <- function(betaVect,y,X,mu,Sigma){
  
  nPara <- length(betaVect);
  linPred <- X%*%betaVect;
  
  # evaluating the log-likelihood                                    
  logLik <- sum( linPred*y -log(1 + exp(linPred)));
  if (abs(logLik) == Inf) logLik = -20000; # Likelihood is not finite, stear the optimizer away from here!
  
  # evaluating the prior
  logPrior <- dmvnorm(betaVect, matrix(0,nPara,1), Sigma, log=TRUE);
  
  # add the log prior and log-likelihood together to get log posterior
  return(logLik + logPrior)
}


initVal <- as.vector(rep(0,dim(X)[2])); 

OptimResults<-optim(initVal,LogPostLogistic,gr=NULL,y,X,mu,Sigma,method=c("BFGS"),control=list(fnscale=-1),hessian=TRUE)

quantile(Data$NSmallChild, probs = c(0.25, 0.975))
hist(Data$NSmallChild)

BetaCoef<- OptimResults$par

J<--solve(OptimResults$hessian)

myconf95<-c(lowbound =BetaCoef[6]-1.96*sqrt(J[6,6]), highbound=BetaCoef[6]+1.96*sqrt(J[6,6]))



######2c

##J as the matrix of covariates for sigma is calculated before as well as BetaCoef which is mu or the BetaCoefficients
Woman<-c(10, 8, 10, 100, 40, 2,0)




logProv<- function(n, betas= BetaCoef, covariances= J,obs=Woman){
  prob<- integer(n)
  Pr<- integer(n)
  for(i in 1:n){
    myrandombetas<- as.vector(rmvnorm(1, mean= BetaCoef, sigma = covariances))
    Pr[i]<- exp((obs)%*%myrandombetas)/(1 + exp((obs)%*%myrandombetas))
    prob[i]<- rbinom(1,1,Pr[i])
  }
  
  return(list("Predictive results"= prob, "Probability results"= Pr))
}


distribution_of_the_predictive_results<- logProv(10000,  betas= BetaCoef, covariances= J,obs=Woman)

hist(distribution_of_the_predictive_results[[1]], 
     xlab = "bernoulli results on 0, 1", 
     ylab = "frequency results", 
     main = "Distribution of the predictive results for 10000 cases")
