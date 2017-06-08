
#Assignment 1

#install.packages("readr")
library(readr)
data <- read.csv("C:/Users/M/Desktop/Statistics and Data Mining Master/Semester 1/Second part of the semester/intro to machine learning/lab4/australian-crabs.csv")

library(ggplot2)
ggplot2::ggplot(data)+
          geom_point(aes(x=CL ,y=RW, color = sex))+
          theme_dark()

          


##Assignment 1.1
#PLot length (CL) versus rear width (RW) where observations are colored by Sex


#ASSIGNMENT 1.2
## It is necessary that the data provider has as name CL, RW and sex as colnames of data
## sex class must be either Male or Female

RegressionClassifier<- function(mydata=data){
 
   ##Creating data frame for the data interested in
  X <- data.frame(mydata[["sex"]], mydata[["CL"]], mydata[["RW"]]) 
  colnames(X)<- c("Y", "CL", "RW")

  ## splitting the data from by male and Female
  X_separated<- split(X, X$Y)
  X_male = split(X, X$Y)$Male ##Male data
  u_male<- c(mean(X_male$CL),mean(X_male$RW)) #mean Male
  S_male=cov(as.matrix(X_male[,2:length(X_male)]))*dim(X_male)[1] #cov of male
  
  X_female = split(X, X$Y)$Female #Female data
  u_female<- c(mean(X_female$CL),mean(X_female$RW)) #mean female
  S_female<- cov(as.matrix(X_female[,2:length(X_female)]))*dim(X_female)[1] # covariance female
  
  
  S=(S_male + S_female)/dim(X)[1] #covariance matrix of the female and male
  
  ## Values of w0 and w1 for Male and female, being w0 one number and w1 two
  w0_male = -1/2*t(u_male)%*%solve(S)%*%u_male + log(dim(X_male)[1]/dim(X)[1]) 
  w1_male = solve(S)%*%u_male
  
  w0_female = -1/2*t(u_female)%*%solve(S)%*%u_female+log(dim(X_female)[1]/dim(X)[1])
  w1_female = solve(S)%*%u_female
  #With the previously calculated numbers, it can be calculated my discrimination rates 
  
  ##regression rest between both regressions
  joint_w0 <- w0_female-w0_male
  joint_w1 <- w1_female-w1_male
  
  ##Desicion boundary information
  slope = -joint_w1[1]/joint_w1[2]
  intercept = - joint_w0/joint_w1[2] 
  
  ##my classification of outcomes
  my_sex_classification <-integer(length(mydata$CL))
  myframe_mysex_classification<- data.frame(Real_sex = as.character(mydata$sex),
                                                my_sex_classification= my_sex_classification,
                                                CL = as.numeric(mydata$CL), 
                                                RW = as.numeric(mydata$RW))
  
  
  
  for(i in 1:length(mydata$CL)){
    #if the number obtained by the regression is bigger then Male
    
    if((intercept+slope*mydata[i,6])>mydata[i,5]){
      myframe_mysex_classification[i,2]<- "Male"
      
    }
    ##else female
    else{
      myframe_mysex_classification[i,2]<- "Female"
      
      
    }
  }
  ##Initializing a list with outputs
  res<- list()
  res[["matrix classification"]]<- myframe_mysex_classification
  res[["Confusion_matrix"]]<- table(True =myframe_mysex_classification[,1], Predicted =myframe_mysex_classification[,2])
  res[["missclassification rate"]]<- (res[["Confusion_matrix"]][1,2]+res[["Confusion_matrix"]][2,1])/sum(res[["Confusion_matrix"]])
  res[["Discrimination function"]]<- list(
                                        male = paste0("male ~ ", w0_male, " + ", w1_male[1]," * CL + ",w1_male[2],"* RW", sep = " "),
                                        female = paste0("female ~ ", w0_female, " + ", w1_female[1]," * CL + ",w1_female[2],"* RW ", sep = " ")
                                        )
  res[["decision boundary"]]<- c(intercept= intercept, slope = slope)
  res[["plot with real data"]] <- ggplot2::ggplot(mydata)+
    geom_point(aes(x=CL ,y=RW, color = sex))+
    geom_abline(slope = slope, intercept = intercept, color = "white")+
    theme_dark()
  res[["plot with my classification"]] <- ggplot2::ggplot(res[["matrix classification"]])+
    geom_point(aes(x=CL ,y=RW, color = my_sex_classification))+
    geom_abline(slope = slope, intercept = intercept, color = "white")+
    theme_dark()
  
  return(res)
  }


a<-RegressionClassifier(mydata = data)

##1.4

###creating the regression
##Female == 1, Male = 0
numericalsex<-ifelse(data$sex == "Female",1,0)

##new data column binding this new variable
newdata<- cbind(data, numericalsex)

logisticR<-glm(formula = sex ~ CL+RW, family= "binomial", data = newdata) 

w0<- logisticR$coefficients[1]
w1CL<- logisticR$coefficients[2]
w2RW<- logisticR$coefficients[3]

intercept<- as.numeric(-w0/w2RW)
slope<- as.numeric(-w1CL/w2RW)
##getting the predicted RW
RW_pred<-round(predict.glm(logisticR, newdata = data, type = "response"))
RW_pred<-ifelse(RW_pred==0, "Female", "Male")

R_classification<- data.frame(Real_sex = as.character(data$sex),
                              Sex_pred= RW_pred,
                              CL = as.numeric(data$CL), 
                              RW = as.numeric(data$RW))


##confusion matrix
conf<-table(True =R_classification$Real_sex, Predicted =R_classification$Sex_pred)
##Missclassification rate
missrate<-(conf[1,2]+conf[2,1])/sum(conf)

Decision_boundary<- paste(intercept = intercept, slope = slope)
Decision_boundary
##Prediction
library(ggplot2)
predicted_plot<-ggplot2::ggplot(R_classification)+
  geom_point(aes(x=CL ,y=RW, color = as.character(Sex_pred)))+
  geom_abline(slope =slope, intercept = intercept, color ="white")+
  theme_dark()

predicted_plot

###Assignment 2



###1
## read data as csv 
data<-read.csv2("C:/Users/M/Desktop/Statistics and Data Mining Master/Semester 1/Second part of the semester/intro to machine learning/lab3/creditscoring.csv", sep =";")

n=dim(data)[1]
set.seed(12345) 
id=sample(1:n, floor(n*0.5)) 
train=data[id,] 

id1=setdiff(1:n, id)
set.seed(12345) 
id2=sample(id1, floor(n*0.25)) 
validation=data[id2,]

id3=setdiff(id1,id2)
test=data[id3,] 




##2

# install.packages("tree")
library(tree)

##Checking the different models for train, test
fit_train_dev <-tree(formula = good_bad~., data = train, split = "deviance")
fit_train_gin <- tree(formula = good_bad~., data = train, split = "gini")

train_predict_dev <- predict(fit_train_dev, newdata = train, type ="class")
train_predict_gin <- predict(fit_train_gin, newdata = train, type ="class")
test_predict_dev <- predict(fit_train_dev, newdata = test, type ="class")
test_predict_gin <- predict(fit_train_gin, newdata = test, type ="class")

train_dev<-table(train$good_bad, train_predict_dev) ##misclassification = 0.212
train_gin<-table(train$good_bad, train_predict_gin) ##misclassification = 0.238
test_dev<- table(test$good_bad, test_predict_dev) ##misclassification = 0.268
test_gin<- table(test$good_bad, test_predict_gin) ##misclassification = 0.372


missclass_train<- c(0.212, 0.238)
missclass_test<- c(0.268, 0.372)
myres<-rbind(missclass_train,missclass_test)
colnames(myres)<- c("deviance", "gini")
##3
#install.packages("rpart.plot")
library("rpart.plot")
plot(fit_train_dev, uniform=TRUE,margin=0)
text(fit_train_dev, use.n=TRUE, all=TRUE, cex=.4)

rpart::
  ##train
  trainScore=rep(0,15)
validationScore=rep(0,15)

for(i in 2:15) {
  prunedTree=prune.tree(fit_train_dev,best=i)
  pred=predict(prunedTree, newdata=validation, type="tree")
  trainScore[i]=deviance(prunedTree)
  validationScore[i]=deviance(pred)
}
plot(2:15, trainScore[2:15], type="b", col="red", ylim=c(250,700), 
     ylab="Total sum of errors", xlab="number of leaves")
points(2:15, validationScore[2:15], type="b", col="blue")
legend("topright", c("train", "validation"), lty=c(1,1), col=c("red","blue"))


best<-prune.tree(fit_train_dev, best = 4) #chosen as best number i equal to 4

plot(best, uniform=TRUE,margin=0)
text(best, use.n=TRUE, all=TRUE, cex=.7)

best$terms
best$y

besttest<-predict(best, newdata = test, type = "class")
table(True =test$good_bad, predicted = besttest) ##missclassification rate = 0.256

##4

library(MASS)
library(e1071)

fit_train_naive <- naiveBayes(formula = good_bad~., data=train)

Yfit_train<- predict(fit_train_naive, newdata=train)
Yfit_test <- predict(fit_train_naive, newdata=test)

train_notweighted_table<-table(true = train$good_bad, predicted= Yfit_train)
train_notweighted_missrate<-
  (train_notweighted_table[1,2]+train_notweighted_table[2,1])/sum(train_notweighted_table)

test_notweighted_table<-table(true = test$good_bad, predicted= Yfit_test)
test_notweighted_missrate<-
  (test_notweighted_table[1,2]+test_notweighted_table[2,1])/sum(test_notweighted_table)


##5


##the decisionboundary is good versus bad = 1/10
decision_prob = 10


test_raw_predict=predict(fit_train_naive, newdata=test, type = "raw")
train_raw_predict=predict(fit_train_naive, newdata=train, type = "raw")


test_myframe<- data.frame(good = test_raw_predict[,"good"], 
                          bad = test_raw_predict[,"bad"],
                          Y_predicted_weighted=NA, 
                          True=test$good_bad)

for(i in 1:length(test$good_bad)){
  if((decision_prob*test_myframe[i,2]>test_myframe[i,1])){
    test_myframe[i,3]<- "bad"
  }
  else{test_myframe[i,3]<- "good"}
}


train_myframe<- data.frame(good = train_raw_predict[,"good"], 
                           bad = train_raw_predict[,"bad"],
                           Y_predicted_weighted=NA, 
                           True=train$good_bad)

for(i in 1:length(train$good_bad)){
  if((decision_prob*train_myframe[i,2]>train_myframe[i,1])){
    train_myframe[i,3]<- "bad"
  }
  else{train_myframe[i,3]<- "good"}
}




test_weigthed_confusionmat<-
  table(true = test_myframe$True, predicted =test_myframe$Y_predicted_weighted)
test_weighted_missrate<-
  (test_weigthed_confusionmat[1,2]+test_weigthed_confusionmat[2,1])/
  sum(test_weigthed_confusionmat)

train_weigthed_confusionmat<-
  table(true = train_myframe$True, predicted =train_myframe$Y_predicted_weighted)
train_weighted_missrate<-
  (train_weigthed_confusionmat[1,2]+train_weigthed_confusionmat[2,1])/
  sum(train_weigthed_confusionmat)

