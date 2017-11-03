mod1=sarima(log(gnp),1,1,0)
mod1
mod2=sarima(log(gnp),0,1,2)
mod2
rs=residuals(mod1$fit)
plot.ts(rs)
hist(rs,20)
pacf(rs)
TSA::runs(rs)

mod1=sarima(rec,5,0,5)
TSA::runs(residuals(mod1$fit))

mod1=sarima(rec,1,0,3)
TSA::runs(residuals(mod1$fit))


trend=time(cmort)
temp=as.numeric(tempr-mean(tempr))
fit=lm(cmort~trend+poly(temp,2)+part, na.action=NULL)
plot(cmort)
et=residuals(fit)
plot(et)

res=sarima(et,2,0,0)
mean(residuals(res$fit)^2)

res2=sarima(cmort,2,0,0, xreg=cbind(trend,temp, temp2=temp^2,part))
res2
mean(residuals(res2$fit)^2)

phi=c(rep(0,11),0.9)
data=arima.sim(list(order=c(12,0,0), ar=phi), n=100)
plot(data)
title("ARIMA(1,0)_12 with phi=0.9")
acf2(data)
ACF=ARMAacf(ar=phi, ma=0, lag.max = 100)
PACF=ARMAacf(ar=phi, ma=0, lag.max = 100, pacf = T)
plot(ACF, type="h", xlab="Lag"); abline(h=0)
plot(PACF, type="h", xlab="Lag"); abline(h=0)

phi=c(rep(0,11),0.8)
theta=-0.5
data=arima.sim(list(order=c(12,0,1), ar=phi, ma=theta), n=100)
par(mfrow=c(1,1))
plot(data)
title("ARMA(1,0)x(1,0)_12")
acf2(data,max.lag = 50)
ACF=ARMAacf(ar=phi, ma=theta, lag.max = 50)
PACF=ARMAacf(ar=phi, ma=theta, lag.max = 50, pacf = T)
par(mfrow=c(2,1))
plot(ACF, type="h", xlab="Lag"); abline(h=0)
plot(PACF, type="h", xlab="Lag"); abline(h=0)


x=1:200
y=sin(2*pi/12*x)+rnorm(200,0,0.1)
plot.ts(y)
acf2(y)
mod=sarima(y,0,0,0,0,1,0, S=12)
plot(residuals(mod$fit))


x=AirPassengers
lx=log(x);dlx=diff(lx);ddlx=diff(dlx,12)
plot.ts(cbind(x,lx,dlx,ddlx), main="")
acf(dlx, lag.max = 50)
acf2(ddlx)

m1=sarima(lx,1,1,1,1,1,0, S=12)
m1$fit
m2=sarima(lx,1,1,1,0,1,1, S=12)
m2$fit

m3=sarima(lx,0,1,1,0,1,1, S=12)
m3$fit

sarima.for(lx,24,0,1,1,0,1,1,12)
