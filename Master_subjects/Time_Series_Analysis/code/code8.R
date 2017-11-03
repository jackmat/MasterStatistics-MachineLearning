library(xts)
library(fGarch)
library(astsa)


par(mfrow=c(2,2))
data=garchSim(spec=garchSpec(model=list(alpha=0.1, beta=0, omega=0.2)), n=500)
plot(data,type="l", main=expression(alpha0==0.2~~alpha1==0.1))
data=garchSim(spec=garchSpec(model=list(alpha=0.5, beta=0, omega=2)), n=500)
plot(data,type="l", main=expression(alpha0==2~~alpha1==0.5))
data=garchSim(spec=garchSpec(model=list(alpha=0.2, beta=0, omega=2)), n=500)
plot(data,type="l", main=expression(alpha0==2~~alpha1==0.2))
data=garchSim(spec=garchSpec(model=list(alpha=0.5, beta=0, omega=0.1)), n=500)
plot(data,type="l", main=expression(alpha0==0.5~~alpha1==0.1))

lgnp=diff(log(gnp))
u=sarima(lgnp,1,0,0)
Resid=residuals(u$fit)
plot(Resid)
acf2(Resid^2)

fit=garchFit(~arma(1,0)+garch(1,0),data=lgnp)
summary(fit)

par(mfrow=c(1,2))
data=garchSim(spec=garchSpec(model=list(alpha=rep(0.01, 5), beta=0, omega=0.01)), n=200)
plot(data,type="l", main=expression(alphai==0.01))
data=garchSim(spec=garchSpec(model=list(alpha=rep(0.1, 5), beta=0, omega=0.1)), n=200)
plot(data,type="l", main=expression(alphai==0.1))


par(mfrow=c(1,2))
data=garchSim(spec=garchSpec(model=list(alpha=0.01, beta=0.9, omega=0.01)), n=200)
plot(data,type="l", main=expression(beta==0.9))
data=garchSim(spec=garchSpec(model=list(alpha=0.01, beta=0.2, omega=0.01)), n=200)
plot(data,type="l", main=expression(beta==0.2))


djiar=diff(log(djia$Close))[-1]
plot.ts(djiar)
grid()
fit=garchFit(~arma(1,0)+garch(1,1),data=djiar, cond.dist="std")
summary(fit)
v=volatility(fit,type="sigma")
plot(time(djiar),djiar, type="l", main="Estimated volatility")
grid()
lines(time(djiar),v, col="red")
predict(fit, n.ahead=40, plot=T, nx=length(djiar))

acf2(djiar^2)
TSA::eacf(djiar^2, ar.max=20, ma.max=20)

fit=garchFit(~arma(1,0)+garch(1,1),data=djiar, cond.dist="std")
par(mfrow=c(2,2))
plot(fit, which=9)
plot(fit, which=10)
plot(fit, which=11)
plot(fit, which=13)


soi.d=resid(lm(soi~time(soi), na.action = NULL))
acf2(soi.d)
fit=arima(soi.d, order=c(1,0,0), include.mean = F)
wt=resid(fit)
#theta(B)=1 phi(B)=ar1 --> pi(B)=phi(B). 
ar1=coef(fit)["ar1"]
yhat=filter(rec, filter=c(1,-ar1), sides=1)
ccf(wt, yhat, na.action = na.omit)

