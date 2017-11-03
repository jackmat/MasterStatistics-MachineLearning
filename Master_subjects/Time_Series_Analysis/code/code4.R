g=ARMAacf(ar=c(0.5,0.1),lag.max = 10)
plot(g, type="n", main=expression(phi^1==0.5~~phi^2==0.1))
lines(g, type="h")
abline(h=0)

?ARMAacf
g=ARMAacf(ar=c(1.5,-0.8),lag.max = 30)
plot(g, type="n", main=expression(phi^1==1.5~~phi^2==-0.8))
lines(g, type="h")
abline(h=0, lty=2)

al=read.csv("Albuquerque.csv")
al1=as.data.frame(al[,c(1,2,7)])
pairs(al1)
m1=lm(PRICE~TAX, data=al1, na.action = na.omit)
PRICEadj=residuals(m1)
m2=lm(SQFT~TAX, data=al1, na.action = na.omit)
SQFTadj=residuals(m2)
plot(PRICEadj,SQFTadj)
corr(cbind(PRICEadj,SQFTadj))


ACF = ARMAacf(ar=c(1.5,-.75, -0.1), ma=0, 24)[-1]
PACF = ARMAacf(ar=c(1.5,-.75, -0.1), ma=0, 24, pacf=TRUE)
par(mfrow=c(1,2))
plot(ACF, type="h", xlab="lag", ylim=c(-1,1)); abline(h=0)
plot(PACF, type="h", xlab="lag", ylim=c(-1,1)); abline(h=0)

PACF = ARMAacf(ar=0, ma=0.8, 24, pacf=TRUE)
par(mfrow=c(1,1))
plot(PACF, type="h", xlab="lag", main=expression(theta==0.8)); abline(h=0)

library(astsa)
plot.ts(rec)
acf2(rec,48)
TSA::eacf(rec)
arima(rec, order=c(2,0,0))
arima(rec, order=c(1,0,3))

?arima
