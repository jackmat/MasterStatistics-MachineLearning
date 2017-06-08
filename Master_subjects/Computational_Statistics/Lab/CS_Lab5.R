##Lab5

##Activity 1
set.seed(12345)

data<- read.csv("C:/Users/M/Desktop/Statistics and Data Mining Master/Semester 2/Computationa Statistics/Lecture 5/lottery.csv", sep = ";")
mydata<-data.frame(Draft_No = data[,5], Day_of_year= data[,4])

###Part1
library(ggplot2)

ggplot(data = mydata)+
    geom_point(aes(x = Day_of_year, y =Draft_No))+
    theme_dark()
##part2
mymodel<-loess("Draft_No~Day_of_year", mydata)
Yhat<-predict(mymodel, mydata$Day_of_year)

##new data frame
orderdat2<-cbind(mydata, Yhat)
ggplot(data = orderdat2)+
  geom_line(aes(x = Day_of_year, y =Yhat))+
  theme_dark()

##part3
library(boot)

Tfunc<- function(data,vn){
  
  d<-as.data.frame(data[vn,])
  mymodel<-loess("Draft_No~Day_of_year", d)
 
  
  Xb<- d[which.max(d[,1]),2]
  Xa<- d[which.min(d[,1]),2]
  Ypred_Xb<-predict(mymodel, Xb)
  Ypred_Xa<-predict(mymodel, Xa)
  T<-(Ypred_Xb-Ypred_Xa)/(Xb-Xa)
  return(T)
}
mypvalue<-integer(10)
for(i in 1:10){
  mypvalue[i]<-mean(boot(data = mydata, statistic = Tfunc, R=2000, sim = "ordinary")$t>0)
}


plot(Tresult)
mytab<- table(abs(Tresult$t)>abs(Tresult$t0))
pvalue1<-sum(abs(Tresult$t)>abs(Tresult$t0))/sum(mytab)##True over True+False

##part4

Permutation_test<- function(B, data){
  n<-dim(data)[1]# nº row
  stat<- numeric(B)
  
  ##Calculating t0
  mymodel<-loess("Draft_No~Day_of_year", data)
  
  Xb<- data[which.max(data[,1]),2]
  Xa<- data[which.min(data[,1]),2]
  Ypred_Xb<-predict(mymodel, Xb)
  Ypred_Xa<-predict(mymodel, Xa)
  
  
  t0<-(Ypred_Xb-Ypred_Xa)/(Xb-Xa)
  
  #calculating t for 1:B samples changing Draft_No
  for(b in 1:B){
    
    data[,2] <- sample(1:n, n)
    mymodel<-loess("Draft_No~Day_of_year", data)
    
    Xb<- data[which.max(data[,1]),2]
    Xa<- data[which.min(data[,1]),2]
    Ypred_Xb<-predict(mymodel, Xb)
    Ypred_Xa<-predict(mymodel, Xa)
    stat[b]<-(Ypred_Xb-Ypred_Xa)/(Xb-Xa)
    
  }
  result<- list(t0=t0, 
                tpermutted= stat, 
                mytab = table(abs(stat)>abs(t0)), 
                pvalue = sum(abs(stat)>abs(t0))/B)
  return(result)
}
mypermutation<-Permutation_test(2000, data = mydata)

class(mypermutation[[4]])
##Part5



alpha<-seq(0.1,10,0.1)
myfunc<- function(alpha, data){
  mydata2<- data.frame(data)
  result<- integer(0)
  for(j in 1:length(alpha)){
    for(i in 1:nrow(mydata2)){
      beta<-rnorm(1,183,10)
      mydata2[i,1]<-max(0, min(alpha[j]*mydata2[i,2]+beta, 366))
      }
    result[j]<- Permutation_test(200, data = mydata2)[[4]]
    }
  return(result)
  }
myres<- myfunc(alpha = seq(0.1,10,0.1), data = mydata)
myres
##Activity 2
##1
pricedata<- read.csv("C:/Users/M/Desktop/Statistics and Data Mining Master/Semester 2/Computationa Statistics/Lecture 5/prices1.csv", sep = ";")
ggplot(pricedata)+ geom_histogram(aes(x=Price, color = "red"),bins = 15)+
            theme_dark()


##2
pricevar<-var(pricedata[,1])
library("boot")



fun<-function(data, vn){
  data<-as.data.frame(data[vn,1])
  mymean<-mean(data[,1])
  return(mymean)
}



bootmean<-boot(pricedata,R=1000, statistic = fun, sim ="ordinary")
hist(bootmean$t)
mean(bootmean$t)
##Confidence interval TO DO 
Percentile_CI<-boot.ci(bootmean, conf = 0.95, type ="perc")
Bca_CI<-boot.ci(bootmean, conf = 0.95, type ="bca")
Normal_CI<-boot.ci(bootmean, conf = 0.95, type ="norm")
plot(bootmean)



VarEstBootstrap<-BootvarEst(pricedata[,1])


BootstrapBiasCorrection<- function(data, B, class="jackknife"){
  T_D<-mean(data)
  data<- as.data.frame(data)
  n<- nrow(data)
  myT<- integer(0)
  if(class == "bootstrap"){
    for(i in 1:B){
      mysample<-sample(1:n, n,replace = TRUE)
      newdata<-data[mysample,]
      myT[i]<-mean(newdata)
    }
  
    BiasCorrection<-2*T_D-1/B*sum(myT)
    varianceEst<- 1/(B-1)*sum((myT-mean(myT))**2)
    result<- list(BiasCorrection=BiasCorrection, 
                  varianceEst=varianceEst)
    return(result)}
  else{
    B<-n ## for B = n
    
    #calculating t for 1:B samples changing Draft_No
    for(i in 1:B){
      myT[i]<-mean(data[-i,])
    }
    
    n<-B
    T_i<-n*T_D-(n-1)*myT
    J_t<- (1/n)*sum(T_i)
    varianceEst<- (1/(n*(n-1)))*sum((T_i-J_t)**2)
     
    result<- list(data = myT,
      varianceEst=varianceEst)
    return(result)
  }
}



bootstrap<-BootstrapBiasCorrection(pricedata[,1],2000, class = "bootstrap")
##jackknife

jackknife<-BootstrapBiasCorrection(pricedata[,1],2000, class = "jackknife")
  # nº row






