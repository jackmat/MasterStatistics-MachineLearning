set.seed(12345)
###1a
s<-14# successes
n<-20# trials
alpha0<-2
beta0<- 2

realsd<-sqrt((alpha0+s)*(beta0+n-s)/(((alpha0+s)+(beta0+n-s))**2*((alpha0+s)+(beta0+n-s)+1)))

result<-integer(0)
mysd<- integer(0)

for(i in 1:10000){
  myres<-rbeta(i,alpha0+s, beta0+n-s)
  result[i]<-mean(myres)
  mysd[i]<- sd(myres)
}

tail(mysd)

myplot<- function(x, second){
  data<-as.data.frame(x)
  require(ggplot2)
  ggplot(data)+geom_line(aes(x=1:length(x), y = x, col= "sd of the sample"))+theme_dark()+
    geom_line(aes(x=1:length(x), y = rep(second,10000), col= "Theorical sd"))+xlab("number of random points for each sample")+
    ylab("sd of the sample")
}
myplot(result,2/3)
myplot(mysd,realsd)


###1.2

mydata<- rbeta(10000,alpha0+s, beta0+n-s)



histogram<- function(x, bins){
  data<-as.data.frame(x)
  require(ggplot2)
  ggplot(data)+geom_histogram(aes(x=x, col= "red"), bins = bins)+theme_dark()
  }

histogram(mydata,40)
density(mydata)


prob_theta_smaller<-sum(mydata<0.4)/length(mydata)
pbeta(0.399999,alpha0+s, beta0+n-s)


##1.3

symbol<- log(mydata/(1-mydata))
density(symbol)
histogram(symbol, 40)



mean(symbol)
################################
betabernoulli<- function(alpha = 2, beta = 2, p =14/20, k ){
  formula<- p**(k+alpha-1)*(1-p)**(1-k+beta-1) 
  return(formula)  
}


x<-seq(0,100,1)
b<-lapply(X=x, betabernoulli(k = X))


####2
install.packages("geoR")
#invChisq
library(geoR)
mydata<-c(14, 25, 45, 25, 30, 33, 19, 50, 34,67)


Tao<- function(mu= 3.5, y){
    taosquared<- sum((log(y)-mu)**2)/length(y)
    return(sqrt(taosquared))
  
}
n<-10000
myres<-rinvchisq(n, Tao(mu = 3.5, y= mydata), scale = 1/df)
theoricaldist<-dinvchisq(n, Tao(mu = 3.5, y= mydata))

##b 

Gini<- 2*dnorm(x, 0, 1)*sigma/sqrt(sigma)


###c






####3

vector<- c(40, 303, 326, 285, 296, 314, 20, 308, 299, 296)

radvector<- c(-2.44,2.14,2.54,1.83,2.02,2.33,-2.79,2.23,-2.79,2.23,2.07,2.02)

mu<- 2.39
lambda<- 1
k<- 1/lambda

vonMissDis<- function(k, data, mu,I){
  formula <- exp(k)*prod(exp(k*cos(data-mu))/(2*pi*I))
  return(formula)
}

results<- sapply(seq(0,10,0.1), vonMissDis(k = seq(0,10,0.1) , I = besselI(k,0), data = radvector, mu = 2.39))


#install.packages("plotrix")
library(plotrix)
radial.plot(results)
