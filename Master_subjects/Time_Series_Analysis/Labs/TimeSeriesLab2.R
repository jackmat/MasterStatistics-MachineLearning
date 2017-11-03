###Time Series Lab 2
set.seed(12345)
library(astsa)


#####1
##a

Ar3<-arima.sim(n = 1000, list(ar = c(0.8, -0.2, 0.1)))
# Use the data and the definition of PACF to compute phi33 
#write your own code that performs linear 
#regressions on necessarily lagged variables and then computes an appropriate correlation. 
#Compare the result with the output of function pacf() and with the theoretical value of 33

#Ar32<-arima.sim(n = 1000, list(ar = c(0.8, -0.2)))


par(mfrow=c(2,1))
pacf(Ar3, lag.max = 50, main = "empyrical AR(3) pacf") #computing the empyrical

g=ARMAacf(ar=c(0.8,-0.2, 0.1),lag.max = 33, pacf=TRUE)#computing the theorical
plot(g, type="n", main=("theoretical acf"), xlab = paste(paste0("Lag with  ", expression(phi^1==0.8~~phi^2==-0.2~~phi^3==0.1))))
lines(g, type="h")
abline(h=0)


##b
Ar2<-arima.sim(n = 100, list(ar = c(0.8, 0.1)))

Ar2Yule<-ar(Ar2, aic = TRUE, method = c("yule-walker"), order = 2)
AR2CSS<-arima0(Ar2, order = c(2,0,0), method = "CSS", optim.control = list(maxit = 1000))
AR2MLE<-arima0(Ar2, order = c(2,0,0), method = "ML", optim.control = list(maxit = 1000))

Yule<-cbind(Ar2Yule$ar, sqrt(Ar2Yule$var.pred))
CSS<- cbind(AR2CSS$coef, sqrt(diag(AR2CSS$var.coef)))
MLE<-cbind(AR2MLE$coef, sqrt(diag(AR2MLE$var.coef)))
colnames(Yule)<- c("estimates", "se")
colnames(CSS)<- c("estimates", "se")
colnames(MLE)<- c("estimates", "se")

list(Yule = Yule, CSS= CSS, MLE = MLE)

ci<- c(MLE[2,1]-1.96*MLE[2,2], MLE[2,1]+1.96*MLE[2,2])
##c
#install.packages("CombMSC")
#install.packages("forecast")
# library(forecast)
# model <- Arima(ts(rnorm(100),freq=4), order=c(1,1,1), seasonal=c(1,1,1),
#                fixed=c(phi=0.5, theta=-0.4, Phi=0.3, Theta=-0.2))
# foo <- simulate(model, nsim=1000)
# fit <- Arima(foo, order=c(1,1,1), seasonal=c(1,1,1))

library(CombMSC)
library(forecast)
par(mfrow=c(1,2))
fit<-sarima.Sim(n = 200, period= 12, model = list(order = c(0,0,1), ma=0.6),list(order= c(0,0,1), ma = 0.3))
THETA <- 0.6
theta <- 0.3
sma1_model <- list(ma = c(theta, rep(0, 10), THETA, theta*THETA))
set.seed(12345)
fit <- arima.sim(sma1_model, 200)


acf(fit, main= "sample acf", lag.max = 30)#sample acf
pacf(fit, main= "sample Pacf", lag.max = 30)#sample pacf
#theoretical acf
theacf=ARMAacf (ma = c(theta,0,0,0,0,0,0,0,0,0,0,THETA, THETA*theta),lag.max=30)
plot (theacf,type="h", main = "theoretical acf")
lines(theacf, type="h")
abline(h=0)
#theoretical pacf
thepacf=ARMAacf (ma = c(theta,0,0,0,0,0,0,0,0,0,0,THETA,THETA*theta),lag.max=30,pacf=TRUE)
plot (thepacf,type="h", main = "theoretical pacf")
lines(thepacf, type="h")
abline(h=0)

tsdisplay(fit)


##d
par(mfrow= c(1,1))
library(forecast)
library(kernlab)

sma1_fit <- arima(fit, order = c(0, 0, 1), seasonal = list(order=c(0, 0, 1), period=12))
sma1_pred <- predict(sma1_fit, n.ahead = 30)
ts.plot(fit, sma1_pred$pred, col = 1:2, xlim = c(1, 230))

U=sma1_pred$pred+pred$se
L=sma1_pred$pred-pr$se
xx=c(time(U),rev(time(U)))
yy=c(L,rev(U))
polygon(xx,yy, border=8, col=gray(0.6, alpha=0.2))
lines(pr$pred, type="p", col=2)    

# fit2<-arima(fit, order = c(0,0,1), seasonal = list(order= c(0,0,1), period = 12))
# fcast <- forecast(fit2, h=30)
# plot(fcast)

?forecast
##Not done the part with GAUSSPR
library(kernlab)
sma1_df <- data.frame(t = 1:200, xt = as.vector(fit))
sma1_fit2 <- gausspr(x=sma1_df$t, y=sma1_df$xt)
sma1_pred2 <- predict(sma1_fit2, newdata = data.frame(1:230))
plot(fit)
lines(sma1_pred2, col="red")

##E
library(tseries)
set.seed(12345)
model<-arima.sim(n = 50, model = list(ar = c(0.7), ma=c(0.5)))
class(model)
fitARMA<- arima(model[1:40], order = c(1,0,1), include.mean = FALSE)

myforecast<-forecast(fitARMA, h = 10)


plot(myforecast, col = "blue")
lines(model, col = "black")
legend("topleft", # places a legend at the appropriate place 
       c("Predicted", "True"), # puts text in the legend
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),col=c("Blue","Black")) # gives the legend lines the correct color and width

#############2
par(mfrow=c(2,2))
data("chicken")

acfpacfplot<- function(data, row, column){
  par(mfrow=c(row,column))
  lag1<-diff(data,1)
  acf(lag1, main= "acf difference", lag.max = 40)
  pacf(lag1, main= "pacf difference", lag.max = 40)
  acf(data, main= "acf", lag.max = 40)
  pacf(data, main= "pacf", lag.max = 40)
  par(mfrow=c(1,1))
  
}


acfpacfplot(ts(chicken),1,2)


##b
data("so2")
data("EQcount")
data("HCT")

acfpacfplot(ts(so2),1,2)
acfpacfplot(ts(EQcount),1,2)
acfpacfplot(ts(HCT),1,2)


#####3
#install.packages("TSA")
library(TSA)
par(mfrow=c(1,1))
##a
data("oil")
plot(oil)
TSA::eacf(oil)

model1<-arima(oil, order = c(1,0,1), optim.control = list(maxit = 1000))
model2<-arima(oil, order = c(2,0,1))


acfpacfplot(oil,2,2)
plot((diff(oil,1)), type = "l")
adf.test(diff(oil,1))#Computes the Augmented Dickey-Fuller test for the null that x has a unit root
##proven that it is stationary
qqnorm(model1$residuals)
abline(0,1)  

qqnorm(model2$residuals) 
abline(0,1)  ##error term not normally distributed, there is staff not taken into account by the model

##run tests
sum(model1$residuals>median(model1$residuals))/length(oil) ##medium: independence result on the test on the data

#Box-Ljung test
Box.test((model1$residuals),type="Ljung",lag=20,fitdf=1)
###H0: The data are independently distributed (i.e. the correlations in the population from which the sample is taken are 0, so that any observed correlations in the data result from randomness of the sampling process).
###Ha: The data are not independently distributed; they exhibit serial correlation.


plot(model1$residuals)

myforecast<-forecast(model1$coef, 20)
plot(myforecast)


#b