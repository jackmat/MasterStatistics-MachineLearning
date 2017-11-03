mod=arima(rec, order=c(2,0,0))
pr=predict(mod,n.ahead=30)
ts.plot(rec, pr$pred, col=1:2, xlim=c(1980,1990))

#NOTE THAT THIS IS NOT 95% prediction band! If you wan 95% take "2*pr$se"
#it is actually easier to use sarima.for for forecasting
U=pr$pred+pr$se
L=pr$pred-pr$se
xx=c(time(U),rev(time(U)))
yy=c(L,rev(U))
polygon(xx,yy, border=8, col=gray(0.6, alpha=0.2))
lines(pr$pred, type="p", col=2)        




#let phi1=1.5, phi2=0.75

data=arima.sim(model=list(ar=c(1.5, -0.75)), n=200)
plot.ts(data)
gamma0=var(data)
rho=acf(data, lag.max = 2)$acf

R=matrix(c(rho[1], rho[2], rho[2],rho[1]), nrow=2)
R
rhop=c(rho[2], rho[3])
rhop

phihat=solve(R,rhop)
phihat

sigma2=gamma0*(1-sum(rhop*phihat))
sigma2

#alternatively
res=ar.yw(data, order=2)
#variance-covariance matrix for coefficients
res$asy.var.coef
res


plot(soi)
TSA::eacf(soi)
res=arima(soi,order=c(2,0,1), method="CSS")
res
plot(residuals(res))
plot(soi)
lines(soi-residuals(res), col="blue")

par(mfrow=c(1,2))
plot.ts(globtemp)
acf(globtemp)

library(tseries)
adf.test(globtemp)

plot(gnp)
plot(diff(gnp))
plot(diff(log(gnp)))
lgnp=diff(log(gnp))
grid()
adf.test(lgnp)
acf2(lgnp)
TSA::eacf(lgnp)

