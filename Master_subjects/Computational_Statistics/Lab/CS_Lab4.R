####LAB 4
## Activity 1

set.seed(12345)
###1.1
posterior_PDFlognorm<- function(value,param){
  return(dlnorm(x=value, meanlog = param, sdlog = 1))
}

##target distribution
targetDist<- function(x){
  return(x**5*exp(-x))
}

MetropolisHastings<- function(times, init, posterior,ydist="rlnorm"){
  result<- integer(0)
  result[1]<-init
  counter<-1
  init<- init
  
  while(counter<times){
    
    x<-init
    if(ydist == "chi"){
      y0<- rchisq(1,floor(x+1))
      }
    else{
      y0<- rlnorm(1, x,1)
    }
    #Sample a candidate value X* from a proposal distribution g(.|x)
    
    myrunif<- runif(1,0,1)
    
    alpha<-(posterior(x, y0)/posterior(y0,x))*(targetDist(y0)/targetDist(x))
    if(myrunif<alpha){
      init<- y0
      
    }else{
      init<-init
    }
    counter<-counter+1
    result[counter]<- init
    
  }
  return(result)
}

MCMH<-MetropolisHastings(2000,1, posterior_PDFlognorm)

library(ggplot2)
myplot<- function(data){
  mydata<- as.data.frame(data)
  mplot<- ggplot2::ggplot(data = mydata)+geom_line(aes( x= 1:nrow(mydata), y = data))+
          ylab("distribution points")+xlab("number of values randomly sampled")+
          theme_dark()
  return(mplot)
}
myplot(MCMH)


###1.2
set.seed(12345)
chisquarePDF<- function(value,param){
  return(dchisq(value,floor(param+1)))
}

MCMH_chi<-MetropolisHastings(2000, 1, chisquarePDF, "chi")
myplot(MCMH_chi)

###1.3 EXplanation

##1.4
#install.packages("coda")
library(coda)
mylist<-mcmc.list()

times<-2000
n<- 10

for( i in 1:n){
  mylist[[i]]<-as.mcmc(MetropolisHastings(times, i, chisquarePDF, "chi"))

}
mylist
print(gelman.diag(mylist))
      

##1.5
Elognormal<- mean(MCMH)
Echi<- mean(MCMH_chi)

##1.6
alpha <- 6 
beta<- 1
Ebeta <- alpha/beta

##Activity 2
library(ggplot2)
load("C:/Users/M/Desktop/Statistics and Data Mining Master/Semester 2/Computationa Statistics/Lecture 4/chemical.RData")

##2.1
mydata<- data.frame(X=X, Y = Y)

ggplot2::ggplot(mydata)+ geom_line(aes(x = X, y = Y))+
          xlab("day of measurement")+ ylab("measured concentration of the chemical")+
          theme_dark()

##2.2

posterior<- function(data, iterations){
  count<- 0
  plikelihood<- integer(0)
  for(j in 1:iterations){
  counter<-0
  
    for(i in 1:length(data)){
      #variables
      counter<- counter+1
      count<- count+1
      y<- data[counter]
      mu1<-mean(data[1:counter])
      mu2<-mean(data[1:(counter+1)])
      mu0<- mean(data[1:(counter-1)])
      if(counter == 1){
        plikelihood[count]<- exp((-1/0.04)*(mu1-((y+mu2)/2)**2))
        }
      if(counter==length(data)){
        plikelihood[count]<- exp((-1/0.04)*(mu1-((y+mu0)/2)**2))
      }
      else{
        plikelihood[count]<- exp((-1/(2*0.04/3))*(mu1-((y+mu0+mu2)/3)**2))
        
      }
    }}
  return(plikelihood) 
}

result<-posterior(mydata[,2], 1000)
myplot(result)

gibbs<- function(data, tmax){
  t<-0
  d<- length(data)
  Xt<-0
  
  
  while(t<tmax){
    t<-t+1
    
    for(i in 2:d){
      y<- data[i-1]
      mu0<- Xt[i-1]
      mu1<-Xt[i]
      mu2<-Xt[i+1]
      
      if(d == 1){
        Xt[t]<- exp((-1/0.04)*(mu1-((y+mu2)/2)**2))/dnorm(mu1,sqrt(0.2))
      }
      if(d==length(data)){
        Xt[t]<- exp((-1/0.04)*(mu1-((y+mu0)/2)**2))/dnorm(mu1,sqrt(0.2))
      }
      else{
        Xt[t]<- exp((-1/(2*0.04/3))*(mu1-((y+mu0+mu2)/3)**2))/dnorm(mu1,sqrt(0.2))
        
      } 
    }
  }
  return(Xt)  
}

result<-gibbs(mydata[,2], 1000, 1)
