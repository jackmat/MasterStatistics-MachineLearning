###Time Series Lab 1
library(stats)
set.seed(12345)
##1a
w_t<-rnorm(100)
x_t1<- integer(100)
x_t2<-integer(100)
for(t in 3:100){
  x_t2[t] <- cos(2*pi*t/5)
  x_t1[t] <- -0.8*x_t1[t-2]+w_t[t]
}
myfilter<- 0.2*(x_t1)
fx_t1<-stats::filter(x_t1, filter = rep(0.2,5), method = "convolution")
fx_t2<-stats::filter(x_t2, filter = rep(0.2,5), method = "convolution")



par(mfrow=c(1,1))
plot(x_t1, type = "l", col= "red", ylim=c(-2,4), lwd= 2)
lines(fx_t1, type = "l", col= "black")
legend("topright", # places a legend at the appropriate place 
       c("without filter", "with filter"), # puts text in the legend
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),col=c("red","black")) # gives the legend lines the correct color and width


plot(x_t2, type = "l", col= "red", ylim=c(-2,4))
lines(fx_t2, type = "l", col= "black")
legend("topleft", # places a legend at the appropriate place 
       c("without filter", "with filter"), # puts text in the legend
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),col=c("red","black")) # gives the legend lines the correct color and width


par(mfrow=c(1,1))

##b 

#HINT1: x_t is related to AR
#HINT2: w_t is related to MA
z1<- c(1,-4,2,0,0,1)
##causal = the absolute value of the roots is >1

causal1 <- abs(polyroot(z1)) #polyroot solves the polyomial
causal1>1 #if(causal>1){print("It is causal")}else{print("it is NOT causal")}
#There are 2 falses. 

z2<- c(1,0,3,0,1,0,-4)
causal2 <- abs(polyroot(z2))
causal2>1


#c
set.seed(54321)
## in order to write it as an arima model, we need to leave the data in the following form:
## x_t = phi*x_{t-1}...+pi_p*x{t-p}+w_t+ theta_1*w_{t-1}+...+theta_q*w{t-q}, recall that w_t does not have coefficient, so in the arima model will be 0

model<-arima.sim(n = 100, model = list(ar = c(-3/4), ma=c(0,-1/9)))

#theoretical
theoreticARMA<-ARMAacf(ar = c(-3/4), ma=c(0,-1/9), lag.max = 20)
plot(theoreticARMA, type = "h")
acf(model , lag.max = 100)
#if the result is inside the acf plot lines, it means that there might be white noise


#2
#a
library(astsa)
Data<-read.csv("C:/Users/Carles/Desktop/MasterStatistics-MachineLearning/Master_subjects/Time_Series_Analysis/Data/Rhine.csv", sep =";", dec = ",")
tsdata<-ts(data = Data$TotN_conc) 
plot(tsdata)
lag1.plot(tsdata, max.lag = 12)
?lag1.plot
rho <- acf(tsdata, type="correlation", plot=T)
par(mfrow=c(3,4))
for(i in 1:12){
  plot(tsdata,lag(x = tsdata, k = i))
}
par(mfrow=c(1,1))


##b
mydata<- cbind(X= 1:168, y=tsdata)
fit = lm(y~X,mydata) # generate linear model
summary(fit)
par(mfrow=c(1, 1))
plot(resid(fit), type='o', main="Detrended")
plot(diff(tsdata), type='o', main='First Difference')

par(mfrow=c(3, 1))
acf(tsdata, 48, main='gtemp')
acf(resid(fit), 48, main='detrended')
acf(diff(tsdata), 48, main='first differences')

tsdata
par(mfrow=c(1,1))

#c
ks2<-ksmooth(time(tsdata), tsdata, 'normal', bandwidth=2)
ks10<-ksmooth(time(tsdata), tsdata, 'normal', bandwidth=10)
plot(tsdata, type='p')
lines(ks2, col = "green")
lines(ks10, col = "red")
legend("topright", # places a legend at the appropriate place 
       c("Kernel = 2", "Kernel = 10"), # puts text in the legend
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),col=c("green","red")) # gives the legend lines the correct color and width

residual2<- ks2$y-tsdata
residual10<- ks10$y-tsdata
par(mfrow= c(1,1))
plot(residual2)
plot(residual10)
acf(residual2)
acf(residual10)

##d


mynewdata1<-cbind(mydata, rep(1:12,14))
colnames(mynewdata1)<- c("x", "y", "month")
res<-lm(y~., data=mynewdata1)
acf(res$residuals)
plot(res$residuals, type = "l")



##e

step<-step(res,direction="both")
step$coefficients
AIC(step)
plot(step$coefficients)
acf(step$residuals)
summary(step)
summary(res)



AIC(step)
AIC(res)

##3


library(astsa) 
#a
plot(oil, type = "l", col= "red", ylim=c(min(oil), max(gas)), lwd= 3, ylab = "prices")
lines(gas, type = "l", col= "black")
legend("topleft", # places a legend at the appropriate place 
       c("oil", "gas"), # puts text in the legend
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),col=c("red","black")) # gives the legend lines the correct color and width


#b

logoil<-log(oil)
loggas<-log(gas)
plot(logoil, type = "l", col= "red", ylim=c(min(logoil), max(loggas)), lwd= 3, ylab = "logprices")
lines(loggas, type = "l", col= "black")
legend("topleft", # places a legend at the appropriate place 
       c("oil", "gas"), # puts text in the legend
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),col=c("red","black")) # gives the legend lines the correct color and width

##c
par(mfrow= c(1,1))
difflogoil<-diff(logoil)
diffloggas<-diff(loggas)
plot(difflogoil, type='o', main='First Difference oil')
plot(diffloggas, type='o', main='First Difference gas')
#par(mfrow=c(3, 1))
acf(diffloggas, 48, main="first differences gas")
acf(difflogoil, 48, main="first differences oil")


x_t<-difflogoil
y_t<-diffloggas
##d
par(mfrow=c(2, 2))

length(x_t)
length(y_t)
lags<-c(0,1,2,3)

for(lag in lags){
  xlag <- lag(x_t, -lag, na.pad = TRUE)
  
  length(y_t)
  smoother<-ksmooth(xlag,y_t, bandwidth = 0.1)
  plot(x= xlag, y = y_t, main = paste0("lag = ", lag), 
       xlab = paste0("log oil lag = " ,lag), ylab = "log gas")
  lines(smoother, col = "red")  
  
}

##e
set.seed(12345)
par(mfrow= c(1,1))
myframe<- ts.intersect(y_t = y_t, x_t = x_t,lag =stats::lag(x_t, k =1), I= as.ts(x_t>0))

colnames(myframe)<- c("y", "x_t", "x_{t-1}","x_tpos")
lmmodel<-lm(y~.+rnorm(length(x_t),0,1), data = myframe)
summary(lmmodel)

lmmodel$coefficients
acf(lmmodel$residuals)
plot(lmmodel$residuals)
