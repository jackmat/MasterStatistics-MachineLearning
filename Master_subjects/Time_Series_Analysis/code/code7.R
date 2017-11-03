plot(jj)
points(jj, col="blue", cex=0.5)
plot(rec)
points(rec, col="blue", cex=0.5)

u1=rnorm(3)
u2=rnorm(3)
w=c(1/4, 1/8, 1/12)

x=1:100
Y=numeric(100)
for (i in 1:3)
Y=Y+u1[i]*cos(2*pi*w[i]*x)+u2[i]*sin(2*pi*w[i]*x)
plot(x,Y, type="l")
grid()


Y=2*cos(2*pi*x*0.1)+6*sin(2*pi*x*0.3)#+rnorm(100,0,3) #if noise is desired
P=Mod(2*fft(Y)/100)^2
plot((x-1)/100,P, type="o", xlab="frequency", ylab="P")

fft(Y)*(2/100)*exp(complex(imaginary = -2*pi*(0:99)/100))

nextn(113)

par(mfrow=c(3,1))
arma.spec(log="no", main="White noise")
arma.spec(ma=0.5, log="no", main=expression(MA(1)~~theta==0.5))
arma.spec(ar=0.5, log="no", main=expression(AR(1)~~phi==0.5))


plot(soi)
soiF=mvspec(soi, log="no")
ind= which.max(soiF$spec)
ind
Upper=2*soiF$spec[ind]/qchisq(0.025,2)
Lower=2*soiF$spec[ind]/qchisq(0.975,2)
c(Lower, Upper)

#take n=1000, m=10
n=1000; m=10
c((2*m+1)/qchisq(0.975,2*(2*m+1)),(2*m+1)/qchisq(0.025,2*(2*m+1)) )

n=10000; m=100
c((2*m+1)/qchisq(0.975,2*(2*m+1)),(2*m+1)/qchisq(0.025,2*(2*m+1)) )


k=kernel("modified.daniell", c(3,3))
soiS=mvspec(soi, kernel=k, log="no")

res=ar.yw(soi, order.max=30)
plot(res$aic, type="o")

spaic=spec.ar(soi, log="no", order=res$order)



