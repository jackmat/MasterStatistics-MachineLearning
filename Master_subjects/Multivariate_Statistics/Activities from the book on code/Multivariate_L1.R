##Lab1Multivariate

##Activity 1
link <- 
  "C:/Users/Carles/Desktop/MasterStatistics-MachineLearning/Master_subjects/Multivariate_Statistics/T1-9.dat"
data<-t(read.table(link))
colnames(data)<- c(data[1,])
data<- data[2:nrow(data),]
##Preparing data
mydata<-apply(data, 2,as.numeric)
mydata<- t(mydata)
colnames(mydata)<-c("hundred", "twohundred", "fourhundred", "eighthundred", "1500", "3000", "marathon")
##preview of mydata
mydata<- as.data.frame(mydata)
mydata$hundred<-mydata$hundred/60
mydata$twohundred<-mydata$twohundred/60
mydata$fourhundred<-mydata$fourhundred/60
hist(mydata[,7], 10)
##1a

mymean <- apply(mydata,2,mean)
mysd   <- apply(mydata,2,sd)

rownames(mydata)
##1b
library(ggplot2)
library(scales)

for(i in 1:ncol(mydata)){
  ggplot(data=mydata, aes(mydata[,1])) + 
    geom_histogram( aes(y = (..count..)/sum(..count..)))+ 
    geom_vline(aes(xintercept = mymean[1]), col = "red") +
    geom_vline(aes(xintercept = quantile(mydata[,1], 0.025)), col = "black")+
    geom_vline(aes(xintercept = quantile(mydata[,1], 0.975)), col = "black")+ 
    ggplot2::xlab(paste0("seconds for running ", colnames(mydata)[1], " meters"))+ ylab("density")
}


##Question 2
##a
CovMat<-cov(mydata)
CorMat<- cor(mydata)

names<- colnames(mydata)

##b
combinations<- expand.grid(names, names)

pairs(CovMat)
pairs(corMat)

##c
library(RColorBrewer)
par(mfrow= c(1,1))
makeProfilePlot <- function(mylist,names)
{
  require(RColorBrewer)
  # find out how many variables we want to include
  numvariables <- length(mylist)
  # choose 'numvariables' random colours
  colours <- brewer.pal(numvariables,"Set1")
  # find out the minimum and maximum values of the variables:
  mymin <- 1e+20
  mymax <- 1e-20
  for (i in 1:numvariables)
  {
    vectori <- mylist[[i]]
    mini <- min(vectori)
    maxi <- max(vectori)
    if (mini < mymin) { mymin <- mini }
    if (maxi > mymax) { mymax <- maxi }
  }
  # plot the variables
  for (i in 1:numvariables)
  {
    vectori <- mylist[[i]]
    namei <- names[i]
    colouri <- colours[i]
    if (i == 1) { plot(vectori,col=colouri,type="l",ylim=c(mymin,mymax),
                       xlab = rownames(mydata),
                       ylab = "minutes of each proof"    ) }
    else         { points(vectori, col=colouri,type="l")                                     }
    lastxval <- length(vectori)
    lastyval <- vectori[length(vectori)]
    text((lastxval-10),(lastyval),namei,col="black",cex=0.6)
    
  }
}
names<- colnames(mydata)
mylist <- list(mydata$hundred,mydata$twohundred, mydata$fourhundred, 
               mydata$eighthundred, mydata$`1500`, mydata$`3000`, 
               mydata$marathon)
makeProfilePlot(mylist[1:(length(mylist)-1)],names[1:(length(mylist)-1)])
makeProfilePlot(mylist[length(mylist)],names[length(mylist)])

names(mydata)
library(ggplot2)
library(reshape2)
data<-mydata
mydata$country = rownames(mydata)
xymelt <- melt(mydata)

colnames(xymelt)
library(plotly)

sp <- ggplot(xymelt,aes(x = value)) + 
  geom_histogram(aes(y = ..density..), col="black")

# Divide by levels of "sex", in the vertical direction
sp + facet_grid(variable~., scales="free_x")

cor(matrix(runif(5), 2, 5))
ggcorr()

