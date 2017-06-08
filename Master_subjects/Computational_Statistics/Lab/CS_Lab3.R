###Assignment 1

mydata <- read.csv("C:/Users/M/Desktop/Statistics and Data Mining Master/Semester 2/Computationa Statistics/Lecture 3/population.csv", sep = ";")


sample_choice <- function(data, choices){
  result<- data.frame(0,0)
  for(i in 1:choices){
    data[["proportion"]]<- data[,2]/sum(data[,2]) ##creating proportions
    data<- data[order(data[,3], decreasing = TRUE),] ##ordering by proportions
    data[1,4]<- data[1,3] ##CUMULATIVE
      for(j in 2:nrow(data)){
        data[j,4]<- data[j,3]+data[j-1,4]
      }
    
    
    mychoice<- runif(1,0,1)
    distances <- data[,4]- mychoice
    result[i,]<- data[which.min(distances),1:2]
    data <- data[-which.min(distances),]
    
  }
  return(result)
} 


choice  <-sample_choice(data = mydata, choice = 20)
colnames(choice)<- c("Cities chosen", "Population")
cities<-choice[,1] 
population20<- choice[,2]
population_total<-mydata[,2]

#### Dont know why it does not work
myplot<- function(data){
  library(ggplot2)
  xlabels<- deparse(substitute(data))
  dat2<- as.data.frame(data)
  
  result<-ggplot2::ggplot(data= dat2)+ 
                  geom_histogram(aes(x=dat2) , bins = 40)+
                  xlab(xlabels)+
                  theme_dark()
  return(result)
}
plot1 <-myplot(data = population20)
plot2<- myplot(data= population_total)


require(gridExtra)

grid.arrange(plot2, plot1, ncol=1)


###Assignment 2 
inverseLaplace<- function(p){
  
  if(p<0.5){
    return(log(2*p))
  }else{
    return(-log(2*(1-p)))
  }
}

n<- 10000
result<-integer(n)
for( i in 1:n){
  result[i]<- inverseLaplace(runif(1,0,1))
}

plot3<- myplot(result)
##


FAcception<- function(times){
  
  alpha<- 1
  mu<- 0
  c<- 2/sqrt(2*pi)*exp(1/2)
  rejcounter<-0
  accounter<-0
  counter <-0
  
  result<-integer(0)
  
  while(accounter<times){
    value<-runif(1,0,1)
    myrandnorm<- inverseLaplace(value)
    
    data<- myrandnorm
    
    formula<- alpha/2 * exp(-alpha* abs(data-mu))
    myrandunif<- runif(1,0,1)
    counter<- counter+1
    if(myrandunif>dnorm(myrandnorm)/(c*formula)){ # myrandorm needs to be changed for dnorm(the number generated on the inverse distribution )
      rejcounter<-rejcounter+1
      
    }else{
      accounter<- accounter+1
      result[accounter]<-myrandnorm
    }}
  myres<- list("information"= c("rejection counter"= rejcounter, "acception counter"= accounter, "total runs" = counter), "rejection rate"= rejcounter/counter ,"results accepted" = result)
  return(myres)  
}  
myresults<- FAcception(2000)


normal_distribution<- rnorm(2000,0,1)
simulated_distribution<- myresults$`results accepted`
myresults$`rejection rate`
plot4 <-myplot(normal_distribution)
plot5<- myplot(simulated_distribution)


grid.arrange(plot4, plot5, ncol=1)
