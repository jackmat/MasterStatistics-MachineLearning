##Advanced ML lab 3
library("mvtnorm")

##1a
x <-  c(0.4)  
y <- c(0.719)
xStar <- seq(-1,1,length=200)
sigma_f<-1
l<-0.3
hyperParam<- c(sigma_f, l)
sigmaNoise<- 0.1
nSim <- 100

###Calculating K from y and x, that are two vector inputs
GaussianKernel<- function(x1,x2, sigma_f =sigma_f, l=l){
  n1<- length(x1)
  n2<- length(x2)
  K<- matrix(NA, n1, n2)
  for(i in 1:n2){
    K[,i]<-  sigma_f^2*exp(-0.5*( (x1-x2[i])/l)^2 )
  }
  return(K)
}

GaussianKernel(c(1,4), c(3,1), sigma = 2, l=1)
GaussianKernel(c(1,4), c(3,1), sigma = 2, l=2)
GaussianKernel(c(1,4), c(3,1), sigma = 2, l=3)
GaussianKernel(c(1,4), c(3,1), sigma = 2, l=4)



SimGP <- function(K,x,y, xStar, nSim, sigmaNoise, ...){
  # Simulates nSim realizations (function) form a Gaussian process with mean m(x) and covariance K(x,x')
  # over a grid of inputs (x)
  n <- length(xStar)
  #if (is.numeric(m)) meanVector <- rep(0,n) else meanVector <- m(xStar)
  

  covMat    <- K(x, x,...)
  covMatdiff2<- K(x, xStar,...)
  covMatStar<- K(xStar, xStar,...)
  L<-t(chol(covMat+ (sigmaNoise^2)*diag(dim(covMat)[2])))
  alpha<- solve(t(L),solve(L,y))
  fStar<- t(covMatdiff2)%*%alpha
  v<-solve(L,covMatdiff2)
  covfStar <- covMatStar-t(v)%*%v
  return(list(mean=fStar, covfStar= diag(covfStar)))
}

##b
fSim <- SimGP(y=y, xStar = xStar, x= x, K=GaussianKernel, nSim = nSim, sigma_f =sigma_f, l=l, sigmaNoise = sigmaNoise )

plot(fSim$mean)
plot(fSim$covfStar)
tablefsim<-cbind(mean = fSim$mean, low= fSim$mean-1.96*sqrt(fSim$covfStar),high= fSim$mean+1.96*sqrt(fSim$covfStar))
plot(x= xStar,y = tablefsim[,1], col = "red", type = "l", ylim = c(-3,5), main = "1 data prior")
lines(x= xStar, y =tablefsim[,2], col ="green")
lines(x= xStar, y = tablefsim[,3], col = "green")
points(x = x, y = y)
legend("topleft", # places a legend at the appropriate place 
       c("Mean distr ", "95% Conf. bands"), # puts text in the legend
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),col=c("Red","Green")) # gives the legend lines the correct color and width

##c

x<-c(x,-0.6)
y<-c(y, -0.044)

fSim <- SimGP(y=y, xStar = xStar, x= x, K=GaussianKernel, nSim = nSim, sigma_f =sigma_f, l=l, sigmaNoise = sigmaNoise )

tablefsim<-cbind(mean = fSim$mean, low= fSim$mean-1.96*sqrt(fSim$covfStar),high= fSim$mean+1.96*sqrt(fSim$covfStar))
plot(x= xStar,y = tablefsim[,1], col = "red", type = "l", ylim = c(-3,3), main = "2 data prior")
lines(x= xStar, y =tablefsim[,2], col ="green")
lines(x= xStar, y = tablefsim[,3], col = "green")
points(x = x, y = y)
legend("topleft", # places a legend at the appropriate place 
       c("Mean distr ", "95% Conf. bands"), # puts text in the legend
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),col=c("Red","Green")) # gives the legend lines the correct color and width

###d
x<- c(-1.0,-0.6,-0.2,0.4,0.8)
y<- c(0.768,-0.044,-0.940,0.719, -0.664)

fSim <- SimGP(y=y, xStar = xStar, x= x, K=GaussianKernel, nSim = nSim, sigma_f =sigma_f, l=l, sigmaNoise = sigmaNoise )

tablefsim<-cbind(mean = fSim$mean, low= fSim$mean-1.96*sqrt(fSim$covfStar),high= fSim$mean+1.96*sqrt(fSim$covfStar))
plot(x= xStar,y = tablefsim[,1], col = "red", type = "l", main = "5 data prior, l = 0.3", ylim = c(-2,2))
lines(x= xStar, y =tablefsim[,2], col ="green")
points(x = x, y = y)
lines(x= xStar, y = tablefsim[,3], col = "green")
legend("topleft", # places a legend at the appropriate place 
       c("Mean distr ", "95% Conf. bands"), # puts text in the legend
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),col=c("Red","Green")) # gives the legend lines the correct color and width

###e

sigma_f<-1
l<-1
hyperParam<- c(sigma_f, l)

fSim <- SimGP(y=y, xStar = xStar, x= x, K=GaussianKernel, nSim = nSim, sigma_f =sigma_f, l=l, sigmaNoise = sigmaNoise )

tablefsim<-cbind(mean = fSim$mean, low= fSim$mean-1.96*sqrt(fSim$covfStar),high= fSim$mean+1.96*sqrt(fSim$covfStar))
plot(x= xStar,y = tablefsim[,1], col = "red", type = "l", ylim = c(-2,3), main = "5 prior l = 1")
lines(x= xStar, y =tablefsim[,2], col ="green")
lines(x= xStar, y = tablefsim[,3], col = "green")
points(x = x, y = y)
legend("topleft", # places a legend at the appropriate place 
       c("Mean distr ", "95% Conf. bands"), # puts text in the legend
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),col=c("Red","Green")) # gives the legend lines the correct color and width

###################################2
# 
# 
# 
# plot(xStar, fSim[1,], type="l", ylim = c(-3,3))
# for (i in 2:dim(fSim)[1]) {
#   lines(xStar, fSim[i,], type="l")
# }
# lines(xStar,MeanFunc(xStar), col = "red", lwd = 3)
# 
# 
# # Plotting using manipulate package
# #install.packages("manipulate")
# library(manipulate)
# 
# plotGPPrior <- function(sigma_f, l, nSim=20){
#   fSim <- SimGP(y=y, xStar = xStar, x= x, K=GaussianKernel, nSim = nSim, sigma_f =sigma_f, l=l, sigmaNoise = sigmaNoise )
#   plot(xStar, fSim[1,], type="l", ylim = c(-3,3), ylab="f(x)", xlab="x")
#   for (i in 2:nSim) {
#     lines(xStar, fSim[i,], type="l")
#   }
#   title(paste('length scale =',l,', sigmaf =',sigma_f))
# }
# 
# manipulate(
#   plotGPPrior(sigma_f, l, nSim = 20),
#   sigma_f = slider(0, 2, step=0.1, initial = 1, label = "SigmaF"),
#   l = slider(0, 2, step=0.1, initial = 1, label = "Length scale, l")
# )
# 

###2

library(kernlab)
Data<- read.csv("https://github.com/STIMALiU/AdvMLCourse/raw/master/GaussianProcess/Code/TempTullinge.csv", header=TRUE,  
                sep = ";")

TData<-data.frame(time = 1:2190, date = Data$date,day =rep(1:365,6), temp = Data$temp)
GaussKernel<- function(sigmaF , ell){
  rval <- function(x,y, sigma_f = sigmaF, l = ell){
    n1 <- length(x)
    n2 <- length(y)
    K <- matrix(NA,n1,n2)
    for (i in 1:n2){
      K[,i] <- sigma_f^2*exp(-0.5*( (x-y[i])/l)^2 )
    }
    return(K)
  }
  
  class(rval) <- "kernel"
  return(rval)
}

SmallData<-  TData[seq(1, nrow(TData), 5), ]
sigma_f<-2
l<-1
X<-matrix(c(1,3,4), ncol = 1)
XStar<-matrix(c(2,3,4), ncol = 1)

GaussianKernel2 <- GaussKernel(sigmaF =  sigma_f, ell = l)
cov_matrix <- kernelMatrix(kernel =  GaussianKernel2, x=X, y = XStar)

x1<-1
x2<-2

K<-GaussianKernel2(x=x1,y= x2)
##b
sigma_f<-20
l <-0.2

x<-SmallData$time
x_prime<- seq(1,365,1)
GaussianKernel3<-GaussKernel(sigmaF = sigma_f, ell = l)
K<-kernelMatrix(GaussianKernel3, x,x_prime)

mylm<-lm(temp~time+ I(time^2), SmallData)
sigma_2_n<-var(mylm$residuals)


GPfit <- gausspr(temp ~ time, data = SmallData , 
                 kernel = GaussianKernel3, 
                 kpar = list(sigma = sigma_f, ell = l), var = sigma_2_n)

myrange<-range(SmallData$time)
Xgrid<- myrange[1]:myrange[2]
meanPred<-predict(GPfit, SmallData)
length(meanPred)
plot(x = SmallData$time, SmallData$temp, type = "l",
     xlab = "Time", ylab = "temp")
lines(SmallData$time, meanPred, col="blue", lwd = 2)


##c

fSim <- SimGP(y=SmallData$temp, xStar = SmallData$time, x= SmallData$time, 
              K=GaussianKernel3, nSim = nSim, 
              sigma_f =sigma_f, l=l, 
              sigmaNoise = sqrt(sigma_2_n ))


tablefsim<-cbind(mean = fSim$mean, low= fSim$mean-1.96*sqrt(fSim$covfStar),high= fSim$mean+1.96*sqrt(fSim$covfStar))
plot(x = SmallData$time, SmallData$temp, type = "l",
     xlab = "Time", ylab = "temp", ylim= c(-40,40))
lines(SmallData$time, meanPred, col="blue", lwd = 2)
lines(x= SmallData$time, y =tablefsim[,2], col ="green")
lines(x= SmallData$time, y = tablefsim[,3], col = "green")
legend("topleft", # places a legend at the appropriate place 
       c( "95% Conf. bands", "meanpredb"), # puts text in the legend
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),col=c("Green", "blue")) # gives the legend lines the correct color and width

####d


mylm<-lm(temp~day+ I(day^2), SmallData)
sigma_2_n<-var(mylm$residuals)
sigma_f<-20
l <- 1.2


GaussianKernel4<-GaussKernel(sigmaF = sigma_f, ell = l)


GPfit <- gausspr(temp ~ day, data = SmallData , 
                 kernel = GaussianKernel4, 
                 kpar = list(sigma = sigma_f, ell = l), var = sigma_2_n)


meanPred2<-predict(GPfit, SmallData)

plot(x = SmallData$time, SmallData$temp, type = "l",
     xlab = "Time", ylab = "temp", ylim= c(-40,80))
lines(SmallData$time, meanPred, col="blue", lwd = 2)
lines(x= SmallData$time, y =tablefsim[,2], col ="green")
lines(x= SmallData$time, y = tablefsim[,3], col = "green")
lines(SmallData$time, meanPred2, col="red", lwd = 2)

legend("topleft", # places a legend at the appropriate place 
       c( "95% Conf. bands", "mean. pred. time", "mean. pred. day"), # puts text in the legend
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),col=c("Green", "blue", "red")) # gives the legend lines the correct color and width



##e

periodicKernel<- function(sigmaF, ell1, ell2, di){
  rval<-function(x1,x2, sigma_f =sigmaF, l_1=ell1, l_2=ell2, d=di){
  
      K<-  sigma_f^2*exp((-2*sin(pi*abs(x1-x2)/d)**2)/l_1^2)*exp((-1/2*abs(x1-x2)**2)/l_2^2)
    
  }
  class(rval) <- "kernel"
  return(rval)
}
sigma_f<-20
l_1<- 1
l_2<-10
d <- 365/sd(SmallData$time)
periodicKernel5<-periodicKernel(sigmaF=sigma_f, ell1=l_1, ell2=l_2, di=d)

GPfit <- gausspr(temp ~ time, data = SmallData , 
                 kernel = periodicKernel5, 
                 kpar = list(sigma = sigma_f, ell = l), var = sigma_2_n)

meanPred3<-predict(GPfit, SmallData)

plot(x = SmallData$time, SmallData$temp, type = "l",
     xlab = "Time", ylab = "temp", ylim= c(-30,70))
lines(SmallData$time, meanPred, col="blue", lwd = 2)
lines(x= SmallData$time, y =tablefsim[,2], col ="green")
lines(x= SmallData$time, y = tablefsim[,3], col = "green")
lines(SmallData$time, meanPred2, col="red", lwd = 2)
lines(SmallData$time, meanPred3, col="orange", lwd = 2)
legend("topleft", # places a legend at the appropriate place 
       c( "95% Conf. bands", "mean. pred. time", "mean. pred. day", "kernelperiodic"), # puts text in the legend
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),col=c("Green", "blue", "red", "orange")) # gives the legend lines the correct color and width



##3


data <- read.csv("https://github.com/STIMALiU/AdvMLCourse/raw/master/GaussianProcess/Code/banknoteFraud.csv", header=FALSE, sep=",") 
names(data) <- c("varWave","skewWave","kurtWave","entropyWave","fraud") 


data[,5] <- as.factor(data[,5]) 
set.seed(111)
SelectTraining <- sample(1:dim(data)[1], size = 1000, replace = FALSE)
model<-gausspr(fraud~varWave+skewWave, data = data[SelectTraining,])
##Part III
data<- data
predictionfunction<- function(data, rows){
  prediction<-predict(model,data[rows,])
  confusionMat<-table(pred=prediction, true = data[rows,5]) # confusion matrix
  Accuracy<-sum(diag(confusionMat))/sum(confusionMat)
  print(confusionMat)
  print(as.numeric(Accuracy))
  
}

predictionfunction(data = data, rows = SelectTraining)

##contour
x1 <- seq(min(data[,"varWave"]),max(data["varWave"]),length=100)
x2 <- seq(min(data["skewWave"]),max(data["skewWave"]),length=100)
gridPoints <- meshgrid(x1, x2)
gridPoints <- cbind(c(gridPoints$x), c(gridPoints$y))

gridPoints <- data.frame(gridPoints)
names(gridPoints) <- c("varWave", "skewWave")
######
probPreds <- predict(model, gridPoints, type="probabilities")
# Plotting for Prob(setosa)
contour(x2,x1,matrix(probPreds[,1],100), 20, xlab = "skewWave", ylab = "varWave", main = "Prob(varWave, Skewwave)  is not fraud(red), fraud(blue)")
points(data[data[,5]==1,"skewWave"],data[data[,5]==1,"varWave"],col="blue")
points(data[data[,5]==0,"skewWave"],data[data[,5]==0,"varWave"],col="red")

###b

test<-setdiff( 1:dim(data)[1], SelectTraining)
probPreds<-predictionfunction(data = data, rows = test)





#c

model<-gausspr(fraud~., data = data[SelectTraining,])
#train
predictionfunction(data = data, rows = SelectTraining)
#test
predictionfunction(data = data, rows = test)
