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
