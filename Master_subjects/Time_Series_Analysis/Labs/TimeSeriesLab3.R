##Lab 3

library(astsa)
##1

data(chicken)
plot(chicken)

#It looks like Quadratic polynomial
####2
mydata<- data.frame(time= 1:length(chicken), x_t= chicken)
model<-lm(x_t~time, mydata)

z_t<-ts(model$residuals)
plot(z_t)


##3
par(mfrow=c(1,1))
n<-length(z_t)
dR<-fft(z_t)
RDF<- dR/sqrt(n)*exp(complex(imaginary = -2*pi*(0:(n-1)/n)))#Real discrete fourier to scale in line 14
periodogram<- Mod(RDF)**2 ##slide 13
plot(x =(0:(n-1))/n,y = periodogram, type = "o")
# threshold<- 100
# which(periodogram>threshold)
tail(periodogram[order(periodogram)], n = 10)
##We consider limit = 100
CIChi<- function(data){
  ind= which.max(data)
  ind
  Upper=2*data[ind]/qchisq(0.025,2)
  Lower=2*data[ind]/qchisq(0.975,2)
  c(Lower, Upper)
}
CIChi(data =periodogram)

#4
newdata<- ifelse(periodogram>100,RDF, 0)
n<-length(newdata)
InvFourier<- integer(n+36)
for(t in 1:(n+36)){
  InvFourier[t]<- 1/sqrt(n)*sum(newdata*exp(complex(imaginary = 2*pi*(0:(n-1))*t/n)))
}

x<- 1:(180+36)

prediction<-predict(model, newdata = data.frame (time = c(1:(180+36))))
predwithRes<-prediction+ as.numeric(InvFourier)
plot(x = x, y = predwithRes, col = "red")
lines(x =mydata$time, y = mydata$x_t, col ="black")
legend("topleft", # places a legend at the appropriate place 
       c("Predicted", "True"), # puts text in the legend
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),col=c("red","Black")) # gives the legend lines the correct color and width

##5
?chicken
k=kernel("modified.daniell", c(2,2))
sm.per=mvspec(z_t, kernel=k, log="no")##smoothed periodogram
Lh<-sm.per$Lh
f<-sm.per$spec
ci<-data.frame(lower =c((2*Lh)*f/qchisq(0.975, df = 2*Lh)),
                    higher= (2*Lh)*f/qchisq(0.025, df = 2*Lh)) 

plot(x= seq(0,0.5, length=length(f)), y =f, col= "red")
lines(x= seq(0,0.5, length=length(f)), y =ci$lower, col= "black")
lines(x= seq(0,0.5, length=length(f)), y =ci$higher, col= "black")
legend("topright", # places a legend at the appropriate place 
       c("fw", "conf int"), # puts text in the legend
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),col=c("red","Black")) # gives the legend lines the correct color and width

    plot(z_t)
##6 
sma1_fit <- sarima(chicken, 2,1,0,0,0,1,12, details = F)

predARIMA<-sarima.for(chicken,n.ahead =36, 2,1,0,0,0,1,12)$pred
class(predARIMA)
length(predARIMA)
plot(x = x, y = predwithRes, col = "red", type ="l")
points(x =mydata$time, y = mydata$x_t, col ="black")
lines(x =181:(180+36), y = predARIMA, col ="GREEN")
legend("topleft", # places a legend at the appropriate place 
       c("Predicted", "True", "ARIMA"), # puts text in the legend
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),col=c("red","Black","GREEN" )) # gives the legend lines the correct color and width
##7
sma1_fit <- sarima(z_t, 3,0,0,0,0,1,12)

specARIMA<-arma.spec(ar = coef(sma1_fit$fit)[1:3], ma = coef(sma1_fit$fit)[4], log= "no")

###############2

library(astsa)
data(oil)

##1
days<-52*9+33
x_t<- diff(log(oil), 1)
z_t<- x_t[1:days]
TSA::eacf(z_t)
acf(z_t)
pacf(z_t)

##2
par(mfrow= c(1,1))
p<-0
q<-3
fit <- sarima(z_t, p,0,q,0,0,0,0)
MA3fit<- arma(z_t, order = c(p,q))
r_t<-ts(MA3fit$residuals)

plot(r_t, type = "l")
r_t2<-r_t**2
acf(r_t2[4:length(r_t2)])
pacf(r_t2[4:length(r_t2)])

library(xts)
#install.packages("fGarch")
library(fGarch)

fit=garchFit(~arma(0,1)+garch(1,0),data=x_t[1:501], include.mean = FALSE)
summary(fit)


##3
par(mfrow=c(2,2))
plot(fit, which = c(7,10,11,13))##choose 7, 10,11,13


##4
v=volatility(fit,type="sigma")
plot(v, col ="red", type = "l", ylim = c(min(z_t), max(z_t)))
lines(z_t, col = "black")

##5
par(mfrow=c(2,1))
pred=garchSim(spec=garchSpec(model=list(alpha=coef[3], beta=0, omega=coef[2], ma= coef[1])), n=500)
plot(x= 1:length(pred),pred, col ="red", type = "l", ylim= c(-0.2,0.2))
plot(z_t, col = "black", type = "l", ylim= c(-0.2,0.2))

#6
par(mfrow=c(1,1))

prediction<-predict(fit, n.ahead=45, plot=T, nx=length(z_t))
prediction
mean<-prediction$meanForecast
upper<-prediction$upperInterval
lower<- prediction$lowerInterval

plot(x = 1:length(x_t), y =x_t, type = "l")
lines(x = (length(x_t)-(length(mean)-1)):length(x_t), y = mean, col = "green")
lines(x = (length(x_t)-(length(mean)-1)):length(x_t), y = upper, col = "red", type = "l")
lines(x = (length(x_t)-(length(mean)-1)):length(x_t), y = lower, col = "red", type = "l")
