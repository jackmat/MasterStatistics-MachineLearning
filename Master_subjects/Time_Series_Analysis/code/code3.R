par(mfrow=c(2,2))
plot(arima.sim(list(order=c(1,0,0), ar=0.9), n=100), ylab="x", main=expression(phi==0.9))
plot(arima.sim(list(order=c(1,0,0), ar=0.2), n=100), ylab="x", main=expression(phi==0.2))
plot(arima.sim(list(order=c(1,0,0), ar=-0.9), n=100), ylab="x", main=expression(phi==-0.9))
plot(arima.sim(list(order=c(1,0,0), ar=-0.2), n=100), ylab="x", main=expression(phi==-0.2))


par(mfrow=c(2,1))
phi=0.9
h=0:30
rho=phi^h
plot(h,rho, main=expression(phi==0.9), type="n")
lines(h,rho, type="h")

phi=-0.9
h=0:30
rho=phi^h
plot(h,rho, main=expression(phi==0.9), type="n")
lines(h,rho, type="h")

par(mfrow=c(2,2))
plot(arima.sim(list(order=c(0,0,1), ma=0.9), n=100), ylab="x", main=expression(theta==0.9))
plot(arima.sim(list(order=c(0,0,1), ma=0.2), n=100), ylab="x", main=expression(theta==0.2))
plot(arima.sim(list(order=c(0,0,1), ma=-0.9), n=100), ylab="x", main=expression(theta==-0.9))
plot(arima.sim(list(order=c(0,0,1), ma=-0.2), n=100), ylab="x", main=expression(theta==-0.2))

par(mfrow=c(2,1))
theta=0.9
rho=c(1,theta/(1+theta^2),0)
h=0:2
plot(h,rho, main=expression(theta==0.9), type="n")
lines(h,rho, type="h")
theta=-0.9
rho=c(1,theta/(1+theta^2),0)
h=0:2
plot(h,rho, main=expression(theta==0.9), type="n")
lines(h,rho, type="h")
lines(c(0,2),c(0,0), lty=2)


par(mfrow=c(2,2))
plot(arima.sim(list(order=c(0,0,2), ma=c(0.9,0.9)), n=100), ylab="x", main=expression(theta^1==0.9~~theta^2==0.9))
plot(arima.sim(list(order=c(0,0,2), ma=c(0.2,0.2)), n=100), ylab="x", main=expression(theta^1==0.2~~theta^2==0.2))
plot(arima.sim(list(order=c(0,0,2), ma=c(-0.9,-0.9)), n=100), ylab="x", main=expression(theta^1==-0.9~~theta^2==-0.9))
plot(arima.sim(list(order=c(0,0,2), ma=c(0.9,-0.2)), n=100), ylab="x", main=expression(theta^1==0.9~~theta^2==-0.2))

x=rnorm(150)
arima(x,order=c(1,0,1))

z=c(1, -0.4,-0.3,-0.2)
polyroot(z)

ARMAtoMA(ar=.9,ma=0.5, 6)
