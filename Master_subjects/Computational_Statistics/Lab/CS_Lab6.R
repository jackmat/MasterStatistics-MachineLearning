#Assignment 6


##1

gen<- function(data){
  myformula<-(data**2/exp(data))- 2*exp(-(9*sin(data))/(data**2+data+1))
  return(myformula)
}


##2

crossover<- function(x){
  return(sum(x)/2)
}


##3

mutate<- function(x){
  return(x**2%%30)
}

##4
myfun<-function(maxiter,mutprob){
  ##a
  x<-0:30
  plot1<-ggplot(as.data.frame(gen(x)))+
    geom_point(aes(x= 0:30, y = gen(x)))+
    theme_dark()
  plot1
  ##b initial population  
  X<- seq(0,30,5)
  ##C
  values<-  gen(X)
  mydata<-data.frame(X= X, Y = values)
  max<- integer(0)
  max[1]<-max(mydata[,2])
  
  ##d
  max(integer(maxiter+1))
  for(i in 1:maxiter){
    ##i
    mychoice<-sample(1:nrow(mydata),2)##
    ##ii
    index<-which.min(mydata[,2])
    ##iii
    mydata[index,1]<-crossover(mydata[mychoice,1])
    if(runif(1)<mutprob){
      mydata[index,2]<-gen(mydata[index,1])
    }else{
      mymut<-mutate(mydata[index,1])
      mydata[index,1]<- mymut
      mydata[index,2]<-gen(mydata[index,1])
    }
    
    max[i+1]<-max(mydata[,2])
    
  }
  
  initialvalues <- as.character(as.numeric(seq(0,30,5) %in% mydata[,1]))##green initial values
  myframe<-data.frame(x1= mydata[,1],y1=mydata[,2],Initial=as.factor(initialvalues),invalues=gen(X))
  plotting<- ggplot(myframe)+
    geom_point(aes(x= x1, y = y1, col="Ending points"))+
    geom_point(aes(x=seq(0,30,5), y =invalues, col="Initial points"))+ coord_cartesian(xlim = c(0, 30), ylim = c(-3, 1))+
    ylab("gen(x)")+ggtitle(paste0("mutprob =",mutprob,", maxiter =", maxiter ,collapse = " "))+
    theme_dark()
  result<- list(maxvalues =max, plot= plotting, thedata =mydata)
  return(result)
  }

library(ggplot2)

set.seed(12345)
gen1001<-myfun(maxiter = 10, mutprob = 0.1)
gen1005<-myfun(maxiter = 10, mutprob = 0.5)
gen1009<-myfun(maxiter = 10, mutprob = 0.9)
gen10001<-myfun(maxiter = 100, mutprob = 0.1)
gen10005<-myfun(maxiter = 100, mutprob = 0.5)
gen10009<-myfun(maxiter = 100, mutprob = 0.9)
library(gridExtra)
genpicture<-grid.arrange(gen1001$plot,gen1005$plot, gen1009$plot, gen10001$plot, gen10005$plot,gen10009$plot, ncol =2)


##2
#1
data<- read.csv("C:/Users/M/Desktop/Statistics and Data Mining Master/Semester 2/Computationa Statistics/Lecture 6/physical1.csv", sep =",")
head(data)
class(data[,1])

mydata<-data[complete.cases(data),]
ggplot2::ggplot(data = mydata)+
    geom_line(aes(x=X,y = Y, col= "Y"))+
    geom_line(aes(x=X,y = Z, col="Z"))+
    theme_dark()

##It declines as  X is larger


#2

Ylikelihood<- function(data){
  return(exp(data/lambda))
}

Zlikelihood<- function(data){
  return(exp(data/(2*lambda)))
}



EM<- function(data, lambda0 =100 , eps=0.001, kmax=1000){
  data[,4]<-!is.na(data[,3])##True =observed, False = unobserved
  dataobs<-data[data[,4]==TRUE,]
  
  k<-1

  lambda<- integer(0)
  lambda[k]<- lambda0
  k<-k+1
  lambda[k]<-sqrt(lambda[k-1]*sum(2*data[,1]*data[,2])-)
  
  while(lambda[k]-lambda[k-1]>eps && (k<(kmax+1))){
    k<-k+1
    lambda[k]<-(2*sum(data[k,1]*data[k,2])+2*length(Ymiss)*lambda[k-1]+sum(data[k,1]*data[k,3]))/(2*length(data[k,2]+length(data[k,3])))
    ##E-step
    k=k+1
    ##Mstep
    muk<- EY/n
    sigma2k<-EY2/n-muk**2

    ##likelihood
    llvalprev<-Ylikelihood(data[,2], muk= muk, sigma2k = sigma2k, n= r)
    k<-k+1
}
  
  return(c(muk, sigma2k,llvarcurr))
}

mutprob<-0.1
maxiter<-10
paste0("mutprob =", mutprob, ", maxiter =", maxiter, collapse = " ")
