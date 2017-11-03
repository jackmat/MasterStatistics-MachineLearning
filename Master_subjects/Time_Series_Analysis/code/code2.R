w=rnorm(200)
x=filter(w, sides=1, c(0,0.9), method = "recursive")
plot.ts(x, main=expression(alpha==0.9))
x=filter(w, sides=1, c(0,-0.9),method = "recursive")
plot.ts(x, main=expression(alpha==-0.9))
x=filter(w, sides=1, c(0,0.2),method = "recursive")
plot.ts(x, main=expression(alpha==0.2))



rho=acf(soi, 5, type="correlation", plot=T)
print(rho)
par(mfrow=c(2,1))
plot(lag(soi,-1), soi, main='lag1', cex=0.5)
plot(lag(soi,-5), soi, main='lag5', cex=0.5)

x1=rnorm(12)
y1=filter(x1,sides=2,filter=c(0.2,0.5,0.2))[c(-1,-12)]
acf(y1,lag.max=5, plot=F)

x1=rnorm(1002)
y1=filter(x1,sides=2,filter=c(0.2,0.5,0.2))[c(-1,-1002)]
acf(y1,lag.max=5, plot=F)



par(mfrow=c(3,1))
plot(cmort, main="Cardiovascular Mortality", xlab="", ylab="")
plot(tempr, main="Temperature", xlab="", ylab="")
plot(part, main="Particulates", xlab="", ylab="")
temp=tempr-mean(tempr)
df=data.frame(temp=as.numeric(temp), cmort, Time=time(cmort), part)
res=lm(cmort~Time+part+poly(temp,2), data=df)
summary(res)
AIC(res)
BIC(res)

step(res)

resd=residuals(res)
resTS=ts(resd, start = c(1970,1), end=c(1979,40), frequency=52)
plot.ts(resTS)

fish=ts.intersect(rec,soi1=lag(soi, -1), soi2=lag(soi,-2), soi3=lag(soi,-3),
                  soi4=lag(soi,-4),soi5=lag(soi,-5), soi6=lag(soi,-6), dframe = T)
fit1=lm(rec~soi1+soi2+soi3+soi4+soi5+soi6, data=fish, na.action = NULL)
summary(fit1)
plot(resid(fit1))

plot.ts(chicken)

fit = lm(chicken~time(chicken), na.action=NULL) # regress chicken on time
par(mfrow=c(2,1))
plot(resid(fit), type="l", main="detrended")
plot(diff(chicken), type="l", main="first difference")
par(mfrow=c(3,1)) # plot ACFs
acf(chicken, 48, main="chicken")
acf(resid(fit), 48, main="detrended")
acf(diff(chicken), 48, main="first difference")

par(mfrow=c(2,1))
plot(jj)
plot(log(jj))


par(mfrow=c(2,1))
plot(soi)
plot(rec)

lag2.plot(soi, rec,8)

wgts=c(rep(1/13,13))
soif=filter(soi, sides=2, filter=wgts)
plot(soi)
lines(soif,lwd=2, col=4)

plot(soi)
lines(ksmooth(time(soi), soi, "normal", bandwidth=1), lwd=2, col=4)


