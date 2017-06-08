
##Assignment 1

setwd("C:/Users/M/Desktop/Statistics and Data Mining Master/Semester 1/Second part of the semester/intro to machine learning/lab3")

library(readxl)

mydata<- read.csv2("C:/Users/M/Desktop/Statistics and Data Mining Master/Semester 1/Second part of the semester/intro to machine learning/lab3/cube.csv")


#1.1

myspline=function(X = mydata$x,Y= mydata$y, knots= c(2,4)){
  m=length(knots)
  n=length(X)
  H=matrix(0,nrow=n,ncol=m+2) #new features
  H[,1]=1
  H[,2]=X
  for(i in 1:m){
    for(j in 1:n){
      #MISSING: Insert equation computing basis function values from X values
      H[j,i+2]<- H[j,2]-knots[i] 
      if(H[j,i+2] < 0) {H[j,i+2] <- 0}
    }
  }
  colnames(H)<- c("intercept", "y", "x-2", "x-4")
  #use H matrix and Y in order to get the optimal basis function coefficients 'beta'
  data<-as.data.frame(cbind(Y,H[,2:(m+2)]))
  beta<-lm(Y~., data = data)$coefficients
  #predicted values
  Yhat=H%*%beta
  MSE<- sum((Yhat-Y)**2)/length(Y)
  ##creating a list to store Beta coefficients and the plot
  res<- list()
  #plot the original and predicted data in one graph
  library(ggplot2)
  dataplot<- data.frame(observations= X,True= Y, Predicted=Yhat)
  myplot<-ggplot2::ggplot(data = dataplot)+
    geom_point(aes(x=observations, y = True, color = "True"))+
    geom_line(aes(x = observations, y = Predicted, color = "Predicted"))+
    theme_bw()
  
  res[["beta.coef"]]<-beta
  res[["predicted.values"]]<-Yhat
  res[["plot predicted vs true"]]<-myplot
  res[["sum squared residuals"]]<-MSE
  
  return(res)
}

myspline()


##3 

mysmooth<-smooth.spline(x= mydata$x, y = mydata$y)
predicted_y<-predict(mysmooth, x = mydata$y)$y

MSE<-sum((predicted_y-mydata$y)**2)/length(mydata$y)
datfram<- data.frame(x= mydata$x, True=mydata$y, Predicted = predicted_y )
myplot2<-ggplot2::ggplot(data = datfram)+
  geom_point(aes(x = x, y = True, color = "True"))+
  geom_point(aes(x = x, y = Predicted, color = "Predicted"))+
  ylab("y observations")+   theme_bw()

myplot2


##Assignment 2

#1


library(readxl)

mydata<- 
  read_excel("C:/Users/M/Desktop/Statistics and Data Mining Master/Semester 1/Second part of the semester/intro to machine learning/lab3/influenza.xlsx")


library(ggplot2)
myplot3<- function(x, variable1, variable2 ){
  datframe<- data.frame(Time= x, Mortality =variable1, Influenza = variable2 )
  ggplot2::ggplot(data = datframe)+
    geom_point(aes(x = Time, y = Mortality, color = "Mortality"))+
    
    geom_point(aes(x = Time, y = Influenza, color = "Influenza"))+
    ylab("expected y")+ggtitle("Time series plot")+
    theme_bw()
}


myplot3(x= mydata$Time, variable1 =mydata$Mortality, variable2 = mydata$Influenza )

#2


library(mgcv)


gm_model<- gam(Mortality~Year+s(Week), data= mydata)
summary(gm_model)
gm_model$coefficients ##Probabilistic model

summary(gm_model)
y_pred<-predict(object = gm_model, newdata = mydata)


##3
myplot4<- function(x, true_y, y_predicted ){
  datframe<- data.frame(x= x, true_y=true_y, y_predicted = y_predicted )
  ggplot2::ggplot(data = datframe)+
    geom_point(aes(x = x, y = true_y, color = "true_y"))+
    geom_point(aes(x = x, y = y_predicted, color = "y_predicted"))+
    ylab("expected y") + ggtitle("Time series plot")+
    theme_bw()
}

myplot4(x = mydata$Time , true_y = mydata$Mortality, y_predicted =y_pred )

#install.packages("rgl")
#install.packages("akima")
library(rgl)
library(akima)



s=interp(x=mydata$Year,y = mydata$Week, fitted(gm_model))
persp3d(s$x,s$y,s$z, col = "red", xlab = "Year", ylab = "Week" , zlab = "Mortality")



MSE<-sum((mydata$Mortality-y_pred)**2)/length(mydata$Mortality)


##4

##Model for low lambda value  = 0.1
gm_model_1<- gam(Mortality~Year+s(Week), data= mydata, sp = 0.1)

##Model for high lambda value  = 100

gm_model_2<- gam(Mortality~Year+s(Week), data= mydata, sp = 100)

gm_model_1$coefficients
gm_model_2$coefficients

###Predicting new values of Y
y_pred_1<-predict(object = gm_model_1, newdata = mydata)
y_pred_2<-predict(object = gm_model_2, newdata = mydata)



## Plotting new values with the old ones
library(ggplot2)
myplot5<- function(x, true_y, y_predicted, y_predicted2 ){
  datframe<- data.frame(x= x, 
                        true_y=true_y, 
                        y_predicted = y_predicted, 
                        y_predicted2= y_predicted2  )
  ggplot2::ggplot(data = datframe)+
    geom_point(aes(x = x, y = true_y, color = "true_y"))+
    geom_point(aes(x = x, y = y_predicted, color = "sp = 0.1"))+
    geom_point(aes(x = x, y = y_predicted2, color = "sp = 100"))+
    ylab("expected y") + ggtitle("Time series plot")+
    theme_bw()
}


myplot5(x = mydata$Time , true_y = mydata$Mortality, 
        y_predicted = y_pred_1, y_predicted2 = y_pred_2 )




###5

residuals<- y_pred - as.vector(mydata$Mortality)

myplot6<- function(x, variable1, variable2 ){
  datframe<- data.frame(x= x, Residuals=variable1, Influenza = variable2 )
  ggplot2::ggplot(data = datframe)+
    geom_point(aes(x = x, y = Residuals, color = "Residuals"))+
    geom_point(aes(x = x, y = Influenza, color = "Influenza"))+
    ylab("expected y") + ggtitle("Time series plot")+
    theme_bw()
}


myplot6(x= mydata$Time, variable1 =residuals, variable2 = mydata$Influenza)



###6
k_infulenza<-length(unique(mydata$Influenza))
k_Year<-length(unique(mydata$Year))
k_Week<-length(unique(mydata$Week))
gm_model_3<- gam(Mortality~s(Year, k=k_Year)+
                   s(Week, k = k_Week)+
                   s(Influenza, k = k_infulenza), data= mydata)

gm_model_3$coefficients

y_pred_3<-predict(object = gm_model_3, newdata = mydata)


myplot4(x = mydata$Time , true_y = mydata$Mortality, y_predicted =y_pred_3 )

MSE3<-sum((as.vector(mydata$Mortality)-y_pred_3)**2)/length(mydata$Mortality)
