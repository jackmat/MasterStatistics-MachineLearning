##########################################LAb1####################################################
##################################################################################################
#install.packages("geoR")
#install.packages("HDInterval")
library("geoR")
library("HDInterval")
#lab 1 Bayesian learning

#1a
x<-rbeta(10000,2+14,2+6)
h<-hist(x,breaks = seq(0,1,0.02))
xfit<-seq(min(x),max(x),length=100) 
yfit<-dbeta(xfit,16,8) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

#1b
pbeta(0.4,16,8)
y<-x[x<0.4]
prob<-length(y)/length(x)


#1c
xodds<-log(x/(1-x))
hist(xodds,breaks = 50,prob=TRUE, main = 'Histogram and density', xlab = expression(paste(log(theta/(1-theta)))))
lines(density(xodds), col="blue", lwd=2)

#2a

# sek <- c(14, 25, 45, 25, 30, 33, 19, 50, 34 ,67)
# 
# n <- length(sek)
# mu <- 3.5
# tausq <- sum((log(sek)- mu)^2) / n
# invchisq_pdf <- function(x, df, scale) {
#   factor1 <- (scale * df / 2)^(df / 2) / gamma(df / 2)
#   factor2 <- exp(-(df * scale) / (2 * x)) / x^(1 + df / 2)
#   factor1 * factor2
# }
# 
# xs <- seq(0.01, 2, 0.01)
# ys <- invchisq_pdf(xs, 10, tausq)



data<-c(14,25,45,25,30,33,19,50,34,67)

tao2<-function(data)
{
  sum((log(data)-3.5)^2)/length(data)
  
}

tao<-tao2(data=data)

sigma2<-rinvchisq(10000,df=10,scale=tao)
hist(sigma2,breaks = 100, prob=TRUE)
x<-seq(from=0, to=10000, by=0.001)
hx<-dinvchisq(x,df=10,scale=tao)

sigma2.histogram = hist(sigma2, breaks = 100, freq = F)
sigma2.ylim.normal = range(0, sigma2.histogram$density, dinvchisq(sigma2,df=10,scale=tao), na.rm = T)
hist(sigma2, breaks = 100, freq = F, ylim = c(0, 5.5), main=expression("Histogram and theoretical density of" ~ sigma^2), xlab = expression(paste(sigma^2)), ylab = 'Density')
curve(dinvchisq(x,df=10,scale=tao), add = T,col="blue")

#2.b
sigma<-sqrt(sigma2)/sqrt(2)
G<-2*pnorm(sigma,0,1)-1
hist(G,freq=F,breaks=50, main='Histogram and kernel density of G')
lines(density(G),col="blue")

#2.c
quantile(G,  probs = c(0.025, 0.975))
#    2.5%     97.5% 
# 0.1739323 0.4152031

hdi(density(G),credMass=0.95)

#lower     upper 
#0.1601422 0.3918670

#3.a

windradians<-c(-2.44,2.14,2.54,1.83,2.02,2.33,-2.79,2.23,2.07,2.02)
posterior<-function(k)
{
  return(exp(-k)*prod(exp(k*cos(windradians-2.39))/(2*pi*besselI(x=k,nu=0))))
  
}


values<-sapply(k,posterior)
plot(k,values,type="l",main="Posterior of " ~ kappa, xlab=expression(paste(kappa)), ylab="Posterior")

#3.b
max(values)
#4.727694e-06
k[which.max(value)]
#2.125
##########################################LAb2####################################################
##################################################################################################
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


##########################################LAb3####################################################
##################################################################################################
library(geoR)
library(mvtnorm)

set.seed(12345)

data<- read.csv("C:/Users/Carles/Desktop/Bayesian learning/Part3/rainfall.dat.txt")


########1A

##Initial Data


nDraws<-1000
data<- unlist(data)
mu_0<- 0
v_0<- 1
sigma2_0<- 1
tau2_0<- 10


Normal_model<- function(nDraws, data,mu_0, v_0, tau2_0, sigma2_0){
  require(geoR)
  ##Initializing data result
  gibbsDraws <- matrix(0,nDraws,2)
  colnames(gibbsDraws)<-c("mu", "sigma2")
  
  #basic variables
  n<- length(data)
  v_n<- n +v_0
  
  #Initializing basic variables for the gibb sampling
  mu<-mu_0
  
  for(i in 1:nDraws){
    
    sigma2 <- rinvchisq(1, df= v_n, scale = (v_0*sigma2_0+ sum((data-mu)**2))/(n +v_0))
    gibbsDraws[i,2] <- sigma2
    
    w<- (n/sigma2)/(n/sigma2+1/tau2_0)
    mu_n<- w*mean(data)+(1-w)*mu_0
    invtau2<- (1/(n/sigma2+1/tau2_0))
    
    mu <-rnorm(1,mu_n, sd = sqrt(invtau2))
    gibbsDraws[i,1]<- mu
    
  }
  
  return(gibbsDraws)
  
  
}

gibbsDraws<-Normal_model(nDraws=nDraws, data= data ,mu_0= mu_0, v_0= v_0, tau2_0= tau2_0, sigma2_0= sigma2_0)
tail(gibbsDraws)

par(mfrow = c(1,3))
plot(gibbsDraws, type = "l", main = "posterior distribution of the data")
plot(gibbsDraws[,1], type = "l", ylab = "mu", main = "convergence of mu")
plot(gibbsDraws[,2], type = "l", ylab = "sigma", main = "convergence of sigma")



hist(gibbsDraws[,1], freq = FALSE, main='Gibbs draws', ylim = c(0,0.5), xlab= "mu")
lines(seq(-2,4,by=0.01),dnorm(seq(-2,4,by=0.01), mean = 1), col = "red", lwd = 3)
plot(cumsum(gibbsDraws[,1])/seq(1,nDraws),type="l", main='Gibbs draws', xlab='Iteration number', ylab='cumulative mean of mu')
lines(seq(1,nDraws),1*matrix(1,1,nDraws),col="red",lwd=3)
acf(gibbsDraws[,1], main='Gibbs draws', lag.max = 20)
par(mfrow = c(1,1))

###############1b

##########    BEGIN USER INPUT #################
# Data options
rawData <- data
x <- as.matrix(data)

# Model options
nComp <- 2    # Number of mixture components

# Prior options
alpha <- 10*rep(1,nComp) # Dirichlet(alpha)
muPrior <- rep(0,nComp) # Prior mean of theta
tau2Prior <- rep(10,nComp) # Prior std theta
sigma2_0 <- rep(var(x),nComp) # s20 (best guess of sigma2)
nu0 <- rep(4,nComp) # degrees of freedom for prior on sigma2

# MCMC options
nIter <- 100 # Number of Gibbs sampling draws

# Plotting options
plotFit <- TRUE
lineColors <- c("blue", "green", "magenta", 'yellow')
sleepTime <- 0.1 # Adding sleep time between iterations for plotting
################   END USER INPUT ###############

###### Defining a function that simulates from the 
rScaledInvChi2 <- function(n, df, scale){
  return((df*scale)/rchisq(n,df=df))
}

####### Defining a function that simulates from a Dirichlet distribution
rDirichlet <- function(param){
  nCat <- length(param)
  thetaDraws <- matrix(NA,nCat,1)
  for (j in 1:nCat){
    thetaDraws[j] <- rgamma(1,param[j],1)
  }
  thetaDraws = thetaDraws/sum(thetaDraws) # Diving every column of ThetaDraws by the sum of the elements in that column.
  return(thetaDraws)
}

# Simple function that converts between two different representations of the mixture allocation
S2alloc <- function(S){
  n <- dim(S)[1]
  alloc <- rep(0,n)
  for (i in 1:n){
    alloc[i] <- which(S[i,] == 1)
  }
  return(alloc)
}

# Initial value for the MCMC
nObs <- length(x)
S <- t(rmultinom(nObs, size = 1 , prob = rep(1/nComp,nComp))) # nObs-by-nComp matrix with component allocations.
theta <- quantile(x, probs = seq(0,1,length = nComp))
sigma2 <- rep(var(x),nComp)
probObsInComp <- rep(NA, nComp)

# Setting up the plot
xGrid <- seq(min(x)-1*apply(x,2,sd),max(x)+1*apply(x,2,sd),length = 100)
xGridMin <- min(xGrid)
xGridMax <- max(xGrid)
mixDensMean <- rep(0,length(xGrid))
effIterCount <- 0
ylim <- c(0,2*max(hist(x)$density))


##Recording mu and sigma2

matmu<- matrix(0, nrow = nIter, ncol = nComp)
colnames(matmu)<- c("mu1", "mu2")

matsigma2<- matrix(0, nrow = nIter, ncol = nComp)
colnames(matsigma2)<- c("sigma1", "sigma2")

matpi<- matrix(0, nrow = nIter, ncol = nComp)
colnames(matsigma2)<- c("pi1", "pi2")


for (k in 1:nIter){
  message(paste('Iteration number:',k))
  alloc <- S2alloc(S) # Just a function that converts between different representations of the group allocations
  nAlloc <- colSums(S)
  print(nAlloc)
  # Update components probabilities
  w <- rDirichlet(alpha + nAlloc)
  matpi[k,]<- w
  
  
  # Update theta's
  for (j in 1:nComp){
    precPrior <- 1/tau2Prior[j]
    precData <- nAlloc[j]/sigma2[j]
    precPost <- precPrior + precData
    wPrior <- precPrior/precPost
    muPost <- wPrior*muPrior + (1-wPrior)*mean(x[alloc == j])
    
    tau2Post <- 1/precPost
    theta[j] <- rnorm(1, mean = muPost, sd = sqrt(tau2Post))
  }
  matmu[k,]<- muPost
  
  
  # Update sigma2's
  for (j in 1:nComp){
    sigma2[j] <- rScaledInvChi2(1, df = nu0[j] + nAlloc[j], scale = (nu0[j]*sigma2_0[j] + sum((x[alloc == j] - theta[j])^2))/(nu0[j] + nAlloc[j]))
  }
  matsigma2[k,]<- sigma2
  
  
  # Update allocation
  for (i in 1:nObs){
    for (j in 1:nComp){
      probObsInComp[j] <- w[j]*dnorm(x[i], mean = theta[j], sd = sqrt(sigma2[j]))
    }
    S[i,] <- t(rmultinom(1, size = 1 , prob = probObsInComp/sum(probObsInComp)))
  }
  
  # Printing the fitted density against data histogram
  if (plotFit && (k%%1 ==0)){
    effIterCount <- effIterCount + 1
    hist(x, breaks = 20, freq = FALSE, xlim = c(xGridMin,xGridMax), main = paste("Iteration number",k), ylim = ylim)
    mixDens <- rep(0,length(xGrid))
    components <- c()
    for (j in 1:nComp){
      compDens <- dnorm(xGrid,theta[j],sd = sqrt(sigma2[j]))
      mixDens <- mixDens + w[j]*compDens
      lines(xGrid, compDens, type = "l", lwd = 2, col = lineColors[j])
      components[j] <- paste("Component ",j)
    }
    mixDensMean <- ((effIterCount-1)*mixDensMean + mixDens)/effIterCount
    
    lines(xGrid, mixDens, type = "l", lty = 2, lwd = 3, col = 'red')
    legend("topleft", box.lty = 1, legend = c("Data histogram",components, 'Mixture'), 
           col = c("black",lineColors[1:nComp], 'red'), lwd = 2)
    Sys.sleep(sleepTime)
  }
  
}

hist(x, breaks = 20, freq = FALSE, xlim = c(xGridMin,xGridMax), main = "Final fitted density")
lines(xGrid, mixDensMean, type = "l", lwd = 2, lty = 4, col = "red")
lines(xGrid, dnorm(xGrid, mean = mean(x), sd = apply(x,2,sd)), type = "l", lwd = 2, col = "blue")
legend("topright", box.lty = 1, legend = c("Data histogram","Mixture density","Normal density"), col=c("black","red","blue"), lwd = 2)

####C Graphical representation
#Data sets for the distribution of mu1, mu2, sigma2_1, sigma_2, pi1, pi2
matmu
matpi
matsigma2
#Data set for the distribution of mu1,  sigma2_1 in the first case



# sampled_simple<-rnorm(1000, mean = mean(gibbsDraws[,1]), sd = sqrt(mean(gibbsDraws[,2])))

mixture_sample<- mean(matpi[,1])*rnorm(1000, mean = mean(matmu[,1]), sd = sqrt(mean(matsigma2[,1])))+ (1-mean(matpi[,1]))*rnorm(1000, mean = mean(matmu[,2]), sd = sqrt(mean(matsigma2[,2])))

hist(data, freq = FALSE, main='Gibbs draws', xlab= "mu", breaks = 30)
lines(density(sampled_simple), col = "red")
lines(density(mixture_sample), col = "yellow")
legend("topright", box.lty = 1, legend = c("Data histogram","Normal density", "Mixture density"), col=c("black","red","yellow"), lwd = 2)




########Question 2
#install.packages("msm")
library(msm)

Data<- read.csv("C:/Users/Carles/Desktop/Bayesian learning/Part2/WomenWork.dat.txt", sep ="", header = TRUE)
glmModel <-   glm(Work ~ 0 + ., data = Data, family = binomial(link = "probit"))
summary(glmModel)

#### Variables
tau <- 10;         # Prior scaling factor such that Prior Covariance = (tau^2)*I
chooseCov <- c(1:8)  # Here we choose which covariates to include in the model


y <- as.vector(Data[,1]); # Data from the read.table function is a data frame. Let's convert y and X to vector and matrix.
X <- as.matrix(Data[,2:ncol(Data)]);
initVal <- as.vector(rep(0,dim(X)[2]));
covNames <- names(Data)[2:length(names(Data))];
X <- X[,chooseCov]; # Here we pick out the chosen covariates.
covNames <- covNames[chooseCov];
nPara <- dim(X)[2];



# Setting up the prior
mu_0    <- as.vector(rep(0,nPara)) # Prior mean vector
Omega_0   <- tau^2*diag(nPara);
beta_0  <- as.vector(rmvnorm(1, mean = mu_0, sigma =Omega_0))
nIter   <- 1000



ProbitFunction<- function(beta_0, mu_0, Omega_0, X, y, n_iter){
  posy_0  <- which(y == 0)
  posy_1  <- which(y == 1)
  nPara <- dim(X)[2]
  
  
  beta<-matrix(0, ncol = nPara, nrow = n_iter)
  beta[1,]<- beta_0
  B<- solve(solve(Omega_0)+t(X)%*%X)
  
  for(i in 1:n_iter){
    beta[i, ]<- rmvnorm(1, as.vector(B%*%(solve(Omega_0)%*%mu_0+t(X)%*%y)), sigma = B)
    
    y[posy_1] <- rtnorm(length(posy_1), mean = as.vector(X[posy_1,]%*%beta[i,]), sd = 1,lower = 0, upper = Inf)
    y[posy_0] <- rtnorm(length(posy_0), mean = as.vector(X[posy_0,]%*%beta[i,]), sd = 1,lower = -Inf, upper = 0)
    
    
    
  }
  return(beta)
}


Betadist<-ProbitFunction(beta_0=beta_0, mu_0= mu_0, Omega_0= Omega_0, X= X, y= y , n_iter =nIter)
colnames(Betadist)<- covNames
dim(Betadist)
BetaFeat<- data.frame(Mean = apply(Betadist, 2, mean), 
                      upper = apply(Betadist, 2, mean)+1.96*apply(Betadist, 2, sd),
                      lower = apply(Betadist, 2, mean)-1.96*apply(Betadist, 2, sd))
par(mfrow = c(2,2))
for(i in 1: 4){
  hist(Betadist[,i], main = paste("Distribution for " ,covNames[i]), xlab = covNames[i], breaks = 30, freq = FALSE)
  abline(v = BetaFeat$lower[i], b = 0, col = "red")
  abline(v = BetaFeat$upper[i], b = 0, col = "red")
  
}
par(mfrow = c(2,2))
for(i in 5:8){
  hist(Betadist[,i], main = paste("Distribution for " ,covNames[i]), xlab = covNames[i], breaks = 30, freq = FALSE)
  abline(v = BetaFeat$lower[i], b = 0, col = "red")
  abline(v = BetaFeat$upper[i], b = 0, col = "red")
  
}




###C

LogPostProbit <- function(betaVect,y,X,mu,Sigma){
  nPara <- length(betaVect);
  linPred <- X%*%betaVect;
  
  # The following is a more numerically stable evaluation of the log-likelihood in my slides: 
  # logLik <- sum(y*log(pnorm(linPred)) + (1-y)*log(1-pnorm(linPred)) )
  logLik <- sum(y*pnorm(linPred, log.p = TRUE) + (1-y)*pnorm(linPred, log.p = TRUE, lower.tail = FALSE))
  
  # evaluating the prior
  logPrior <- dmvnorm(betaVect, matrix(0,nPara,1), Sigma, log=TRUE);
  
  # add the log prior and log-likelihood together to get log posterior
  return(logLik + logPrior)
  
}

OptimResults<-optim(initVal,LogPostProbit,gr=NULL,y,X,mu= mu_0,Sigma= Omega_0,method=c("BFGS"),control=list(fnscale=-1),hessian=TRUE)

BetaCoef<- OptimResults$par
names(BetaCoef)<- covNames
J<--solve(OptimResults$hessian)

myconf95<-c(lowbound =BetaCoef[6]-1.96*sqrt(J[6,6]), highbound=BetaCoef[6]+1.96*sqrt(J[6,6]))

sampleBetaProbit<- matrix(ncol = length(BetaCoef), nrow = 1000)
for(i in 1:length(BetaCoef)){
  sampleBetaProbit[,i]<- rnorm(1000, mean = BetaCoef[i], sd = sqrt(J[i,i]))
}

par(mfrow = c(2,2))
for(i in 1: 4){
  hist(Betadist[,i], main = paste("Distribution for " ,covNames[i]), xlab = covNames[i], breaks = 30, freq = FALSE)
  abline(v = BetaFeat$lower[i], b = 0, col = "red")
  abline(v = BetaFeat$upper[i], b = 0, col = "red")
  lines(density(sampleBetaProbit[,i]), col = "blue")
}
for(i in 5:8){
  hist(Betadist[,i], main = paste("Distribution for " ,covNames[i]), xlab = covNames[i], breaks = 30, freq = FALSE)
  abline(v = BetaFeat$lower[i], b = 0, col = "red")
  abline(v = BetaFeat$upper[i], b = 0, col = "red")
  lines(density(sampleBetaProbit[,i]), col = "blue")
  
  
}

ConclTAble<- data.frame(GibbsMean = apply(Betadist, 2, mean), 
                        Gibbssd = apply(Betadist, 2, sd), 
                        OptimalBeta = BetaCoef,
                        Gibbssd=sqrt(diag(J)))

##########################################LAb4####################################################
##################################################################################################
### LAB 4

#1
Data<- read.csv("C:/Users/Carles/Desktop/Bayesian learning/Part4/eBayNumberOfBidderData.dat.txt", sep = "")

##a
#### Variables

PoisModel<-glm(nBids~.-Const, family="poisson", data = Data)
logLik(PoisModel)


mysum<-summary(PoisModel)[["coefficients"]]



##Significants at a 95% level
significant <- mysum[which(mysum[,4]<0.05),]
non_significant <- mysum[which(mysum[,4]>=0.05),]
list(significant = significant, nonsignificant = non_significant)


##b
chooseCov <- 1:(length(names(Data))-1);  # Here we choose which covariates to include in the model


y <- as.vector(Data[,1]); # Data from the read.table function is a data frame. Let's convert y and X to vector and matrix.
X <- as.matrix(Data[,2:length(names(Data))]);
covNames <- names(Data)[2:length(names(Data))];
X <- X[,chooseCov]; # Here we pick out the chosen covariates.
covNames <- covNames[chooseCov];
nPara <- dim(X)[2]


##prior Beta
library(geoR)
library(mvtnorm)


mu_0    <- matrix(0, nPara,1)
Sigma_0 <- 100*solve(crossprod(X,X))
Beta_0  <- rmvnorm(1, mean = mu_0, sigma = Sigma_0)
InitVal <- matrix(0, ncol=nPara, nrow =1)


LogPostPois <- function(betaVect,y = y,X = X, mu =mu_0,Sigma= Sigma_0){
  nPara <- length(betaVect);
  linPred <- X%*%betaVect;
  
  # The following is a more numerically stable evaluation of the log-likelihood in my slides: 
  # logLik <- sum(y*log(pnorm(linPred)) + (1-y)*log(1-pnorm(linPred)) )
  logLik <- sum(linPred*y-exp(linPred))
  
  # evaluating the prior
  
  logPrior <- dmvnorm(betaVect, mu, Sigma, log=TRUE);
  
  # add the log prior and log-likelihood together to get log posterior
  return(logLik + logPrior)
  
}

OptimResults<-optim(InitVal,LogPostPois,gr=NULL,y = y,X = X, mu =mu_0,Sigma= Sigma_0 ,method=c("BFGS"),control=list(fnscale=-1),hessian=TRUE)

BetaCoef<- OptimResults$par
colnames(BetaCoef)<- covNames
J<--solve(OptimResults$hessian)

mycoefvar<- data.frame(Coefficients= as.vector(BetaCoef), variance = diag(J))
rownames(mycoefvar)<- covNames
mycoefvar
######C

set.seed(12345)

LogPostPoisson <- function(theta, priormu, priorsigma, X, Y, ...) {
  require(mvtnorm)
  likelihood <- dpois(Y, lambda = as.vector(exp((X) %*% t(theta))), log = TRUE)
  prior <- dmvnorm(theta, mean = priormu, sigma = priorsigma, log=TRUE)
  return(sum(likelihood) + prior)
}


metropl<-function(logPostFunc, theta_0, constant, sigma, nIter,...){
  #initialize chain
  require(mvtnorm)
  theta<- matrix(NA, nrow = nIter+1, ncol = dim(sigma)[1])
  theta[1,]<- theta_0
  rej_rate<-0
  for(i in 2:(nIter+1)){
    new_theta<- rmvnorm(1, mean = theta[i-1,], sigma = constant*sigma)
    cur_theta<-t(as.matrix(theta[i-1,]))
    U<-runif(1,0,1)
    num<-logPostFunc(new_theta,...)+dmvnorm(cur_theta, mean =new_theta, sigma = sigma, log = TRUE)
    den<-logPostFunc(cur_theta,...)+dmvnorm(new_theta, mean =cur_theta, sigma = sigma, log = TRUE)
    
    if(U<min(1,exp(num-den))){
      theta[i,] <-new_theta
    }else{
      theta[i,] <-cur_theta
      rej_rate  <-rej_rate+1
    }}
  
  
  myres<- list(theta= theta, rej_rate= rej_rate/nIter)
  return(myres)
}


res<-metropl(logPostFunc=LogPostPoisson, 
             theta_0= rep(0,nPara), 
             constant= 0.6, 
             sigma = J ,
             priormu= mu_0, 
             priorsigma= Sigma_0, 
             X= X, Y=y, 
             nIter= 10000)


res[[2]]

compbetas<- data.frame(OptimCoefficients= as.vector(BetaCoef), MCMCCoef =colMeans(res[[1]]))
rownames(compbetas)<- covNames
compbetas
##d
Xpred <- matrix(c(1, 1, 1, 1, 0, 0, 0, 1, 0.5), nrow = 1)
predsamples <- rpois(10000, lambda = exp(Xpred %*% t(res[[1]])))
hist(predsamples, freq = FALSE)

##probability of 0 over total
length(which(predsamples==0))/length(predsamples)
