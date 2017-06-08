## Example due to Thomas Ericsson in his Numerical Analysis course at Chalmers
f1<-function(x){(x-1)^6}
f2<-function(x){1-6*x+15*x^2-20*x^3+15*x^4-6*x^5+x^6}
x<-seq(from=0.995,to=1.005,by=0.0001)
y1<-f1(x)
y2<-f2(x)
plot(x,y1,pch=19,cex=0.5,ylim=c(-5*10^(-15),20*10^(-15)),main="Two ways to calculate (x-1)^6",xlab="x",ylab="y")
points(x,y2,pch=18,cex=0.8)
