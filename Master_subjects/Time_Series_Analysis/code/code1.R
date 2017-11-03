#code is primarily taken from Shumway and Stoffer (2017)

plot(USAccDeaths, ylab="Number of deaths", type="o")
plot(as.vector(USAccDeaths)[1:71],as.vector(USAccDeaths)[2:72], type="p", xlab=expression(x^t),ylab=expression(x^{t+1}))
#install.packages("astsa")
library(astsa)
plot(jj, type="o", ylab="Quarterly Earnings per Share")
grid()

plot(globtemp, type="o", ylab="Global Temperature Deviations")
grid()

plot(speech)
grid()
#install.packages("xts")
library(xts)
djiar=diff(log(djia$Close))[-1]
plot(djiar, main="DJIA Returns", type="n" )
lines(djiar)

par(mfrow=c(2,1))
plot(soi, ylab="", xlab="", main="Southern Oscillation Index")
plot(rec, ylab="", xlab="", main="Recruitment")

X=seq(-3,3, by=0.1)
Y=4*cos(X+2)+13*cos(20*X+4)+2*cos(0.2*X+1)+13*cos(15*X-4)+rnorm(length(X),0,5)
plot(X,Y, type="o")
grid()

plot(gnp)
grid()


lgnp=diff(log(gnp))
plot(lgnp)
grid()
regr=ar.ols(lgnp, order=2, demean=F, intercept = T)
fore=predict(regr, n.ahead=20)
ts.plot(lgnp, fore$pred, col=1:2, xlim=c(1980, 2020))
U=fore$pred+fore$se; L=fore$pred-fore$se
xx=c(time(U), rev(time(U))); yy=c(L, rev(U))
polygon(xx,yy, border=8, col=gray(.6, alpha=.2))


Time=1:100
Y=rnorm(length(Time), 0,1)
plot(Time,Y,type="o")

w=rnorm(100,0,1)
xt=filter(w, sides=2, filter=c(0.2,0.5,0.2))
plot.ts(xt, ylim=c(-3,3), main="Moving average")

w = rnorm(550,0,1) # 50 extra to avoid startup problems
x = filter(w, filter=c(1,-.9), method="recursive")[-(1:50)]
plot.ts(x, main=expression(paste(alpha, "=-0.9")))
x = filter(w, filter=c(1,-0.1), method="recursive")[-(1:50)]
plot.ts(x, main=expression(paste(alpha, "=-0.1")))

w = rnorm(200,0,1)
x = cumsum(w) 
wd = w +.2
xd = cumsum(wd)
plot.ts(xd, main="random walk")
lines(x, col="blue")

#ACF
x0=rnorm(10000)
x1=0.4*x0+rnorm(10000)
x2=0.4*x1+rnorm(10000)
x3=0.4*x2+rnorm(10000)
autocov=c(cov(x0,x1), cov(x0,x2), cov(x0,x3))
par(mfrow=c(1,3))
plot(x0,x1)
plot(x0,x2)
plot(autocov, type="n")
lines(autocov, type="h")
