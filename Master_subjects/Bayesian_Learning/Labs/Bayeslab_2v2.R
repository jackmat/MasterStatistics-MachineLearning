#lab 2 Bayesian learning

# install.packages("mvtnorm")
# install.packages("MASS")
library("mvtnorm")
library("geoR")
library("MASS")

data<-read.table("~/Google Drive/Kurser/Bayesian learning/Lab2/TempLinkoping2016.txt",head=TRUE)
data<- read.csv("C:/Users/Carles/Desktop/Bayesian learning/Part2/TempLinkoping2016.txt", sep ="\t")

#1.a
lm<-lm(temp~time+I(time^2),data)
summary(lm)

#1.b
mu_0<-c(-10,93,-85)
sigma_0<-16
v_0<-6
omega_0<-diag(3)
sigma_prior<-rinvchisq(1,df=v_0,scale=sigma_0)
beta_prior<-rmvnorm(n=1, mean =mu_0, sigma = sigma_prior*ginv(omega_0))
regress_prior<-beta_prior[1]+beta_prior[2]*data$time+beta_prior[3]*(data$time)^2
plot(y=data$temp,x=data$time)
lines(y=regress_prior,x=data$time,col="blue")

betasprior<-function(n)
{ 
  beta0<-numeric(n)
  beta1<-numeric(n)
  beta2<-numeric(n)
  
  for (i in 1:n)
  { 
    beta0[i]<-as.vector(rmvnorm(n=1, mean =mu_0, sigma = sigma_prior*ginv(omega_0)))[1]
    beta1[i]<-as.vector(rmvnorm(n=1, mean =mu_0, sigma = sigma_prior*ginv(omega_0)))[2]
    beta2[i]<-as.vector(rmvnorm(n=1, mean =mu_0, sigma = sigma_prior*ginv(omega_0)))[3]
    
     }
  return(data.frame(beta0, beta1, beta2))
}
betasprior<-betasprior(10000)
apply(betasprior, 2, mean)
dim(betasprior)
par(mfrow=c(1,3))

hist(betasprior[,1], breaks = 30, xlab = "Beta1", main = "Distribution of Beta1")
hist(betasprior[,2], breaks = 30, xlab = "Beta2", main = "Distribution of Beta2")
hist(betasprior[,3], breaks = 30, xlab = "Beta3", main = "Distribution of Beta3")



#1.c
X<-cbind(rep(1,nrow(data)),data$time,(data$time)^2)
#beta_hat<-ginv(t(X)%*%X)%*%t(X)%*%data$temp
mu_n<-ginv(t(X)%*%X+omega_0)%*%(t(X)%*%data$temp+omega_0%*%mu_0)
Sigma_n<-t(X)%*%X+omega_0
v_n<-v_0+nrow(data)
sigma_n<-(1/v_n)*(v_0*sigma_0+(t(data$temp)%*%data$temp+t(mu_0)%*%omega_0%*%mu_0-t(mu_n)%*%Sigma_n%*%mu_n))

sigma_posterior<-as.vector(rinvchisq(1,df=v_n,scale=sigma_n))
beta_posterior<-rmvnorm(n=1, mean =mu_n, sigma = sigma_posterior*ginv(Sigma_n))
regress_posterior<-beta_posterior[1]+beta_posterior[2]*data$time+beta_posterior[3]*(data$time)^2
plot(y=data$temp,x=data$time)
lines(y=regress_posterior,x=data$time,col="blue")

betas<-function(n)
{ 
  beta0<-numeric(n)
  beta1<-numeric(n)
  beta2<-numeric(n)
   
  for (i in 1:n)
  { 
    beta0[i]<-as.vector(rmvnorm(n=1, mean =mu_n, sigma = sigma_posterior*ginv(Sigma_n)))[1]
    beta1[i]<-as.vector(rmvnorm(n=1, mean =mu_n, sigma = sigma_posterior*ginv(Sigma_n)))[2]
    beta2[i]<-as.vector(rmvnorm(n=1, mean =mu_n, sigma = sigma_posterior*ginv(Sigma_n)))[3]
    
    }
  return(data.frame(beta0,beta1, beta2))
  }


betas<-betas(1000)
betas0<-betas[,1]
betas1<-betas[,2]
betas2<-betas[,3]


temp_posterior<-function()
{
  n=1000
  k=366
  temp_posterior= matrix(data=NA, nrow=n, ncol=k)
  for(j in 1:k){
    for(i in 1:n){
      temp_posterior[i,j] = betas0[i]+betas1[i]*data$time[j]+betas2[i]*(data$time[j])^2
    }
  }
  return(temp_posterior)
  }
temp_posterior<-temp_posterior()

quantile_f<-function()
{
  k=366
  q_lower<-numeric(k)
  q_upper<-numeric(k)

  for (i in 1:k)
  {
    q_lower[i]=quantile(temp_posterior[,i],  probs = c(0.025, 0.975))[1]
    q_upper[i]=quantile(temp_posterior[,i],  probs = c(0.025, 0.975))[2]
    
    }
  return(data.frame(q_lower,q_upper))
}

quantile_f<-quantile_f()
plot(q_lower,type="l")


par(mfrow= c(1,1))
plot(y=data$temp,x=data$time)
lines( y =quantile_f[,2],x=data$time, col= "red")
lines(y =quantile_f[,1],x=data$time, col = "red")
lines(y=regress_posterior,x=data$time,col="blue")


myfunc<- function (data){
  regress_posterior<-betas[1]+betas[2]*data+betas[3]*(data)^2
  return(regress_posterior)
  
}

temp_mean<-mean(beta_posterior[,1])+mean(beta_posterior[,2])*data$time+mean(beta_posterior[,3])*data$time^2
t<-c(timeMax=data$time[which.max(temp_mean)], tempMax=max(temp_mean))


temp_posterior_max<-function()
{
  n=1000
  
  temp_posterior_max= numeric(n)
 
    for(i in 1:n){
      temp_posterior_max[i] = betas[i,1]+betas[i,2]*t[1]+betas[i,3]*t[1]^2
    }
  
  return(temp_posterior_max)
}

hist(temp_posterior_max())

##1d

lm7<-lm(temp~time+I(time^2)+I(time^3)+I(time^4)+I(time^5)+I(time^6)+I(time^7),data)
mu7<-lm7$coefficients
sigma7<-16
v7<-365
#First values
lambdaplot<-function(lambda){
  lambda <-0.5
  Sigma7<-diag(8)*lambda
  sigma_prior7<-rinvchisq(1,df=v7,scale=sigma7)
  regress_prior7<- matrix(nrow = 1000, ncol = 366)
  for(i in 1:1000){
  beta_prior7<-rmvnorm(n=1, mean =mu7, sigma = sigma_prior7*ginv(Sigma7))
  regress_prior7[i,]<-beta_prior7[1]+beta_prior7[2]*data$time+beta_prior7[3]*(data$time)^2+beta_prior7[4]*(data$time)^3+beta_prior7[5]*(data$time)^4+beta_prior7[6]*(data$time)^5+beta_prior7[7]*(data$time)^6+beta_prior7[8]*(data$time)^7
  
  }
  return(colMeans(regress_prior7))
}
lambda05<-lambdaplot(lambda =0.5)
lambda1<-lambdaplot(lambda =1)
lambda5<-lambdaplot(lambda =10)
lambda10<-lambdaplot(lambda =100)

#lambda<-seq(from=0,to=10,by=1)
par(mfrow = c(2,2))
plot(y=data$temp,x=data$time, main = "lambda = 0.5")
lines(y=lambda05,x=data$time,col="red")
plot(y=data$temp,x=data$time, main = "lambda = 1")
lines(y=lambda1,x=data$time,col="red")
plot(y=data$temp,x=data$time, main = "lambda = 10")
lines(y=lambda5,x=data$time,col="red")
plot(y=data$temp,x=data$time, main = "lambda = 100 ")
lines(y=lambda10,x=data$time,col="red")

####################################################################
####2a
Womenwork<-read.table("~/Google Drive/Kurser/Bayesian learning/Lab2/WomenWork.dat.txt",head=TRUE)
Womenwork<- read.table("C:/Users/Carles/Desktop/Bayesian learning/Part2/WomenWork.dat.txt",head=TRUE)


glmModel <-   glm(Work ~ 0 + ., data = Womenwork, family = binomial)

summary(glmModel)

######b
#install.packages("mvtnorm") # Loading a package that contains the multivariate normal pdf
library("mvtnorm") # This command reads the mvtnorm package into R's memory. NOW we can use dmvnorm function.

# Loading data from file
#Data<-read.csv("/home/carsa564/Desktop/Bayesian learning/WomenWork.dat.txt", sep = "") 
Data<- read.csv("C:/Users/M/Desktop/Statistics and Data Mining Master/Semester 2/Bayesian Learning/lab2/WomenWork.dat.txt", sep = "")
Data<-read.csv("~/Google Drive/Kurser/Bayesian learning/Lab2/WomenWork.dat.txt",sep ="")
Data<- read.csv("C:/Users/Carles/Desktop/Bayesian learning/Part2/WomenWork.dat.txt",sep ="")

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


