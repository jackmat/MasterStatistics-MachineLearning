####Assignment 1
#a
##Seting working directory for the documents

setwd("C:/Users/M/Desktop/Statistics and Data Mining Master/Semester 1/Second part of the semester/intro to machine learning/lab_1")


library(readxl)

##Reading table

data<- read_excel("C:/Users/M/Desktop/Statistics and Data Mining Master/Semester 1/Second part of the semester/intro to machine learning/lab_1/spambase.xlsx")
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,] 


 
col_length<-ncol(data)-1
train_complete<-train[,1:(col_length+1)] #used for the kknn algorithm of the package
train<- train[,1:col_length] #used for our algorythm

test_complete<- test[,1:(col_length+1)] #used for the kknn algorithm of the package
test<-  test[,1:col_length] #used for our algorythm

#Columns to compare and train and test on our algorythm
train_spam = data[id,ncol(data)]
test_spam = data[-id,ncol(data)]

###2
knearest<- function( data, K , newdata, prob){
  data<- as.matrix(data)
  mytest <- as.matrix(newdata)
  
  x_est <- data/sqrt(rowSums(data**2))
  y_est <- mytest/sqrt(rowSums(mytest**2))
  
  matrix_c <- x_est %*% t(y_est) 
  D <- 1 - matrix_c #Distance  cosines computed
  
  K_matrix <- t(apply(D, 2, order)[ 1:K, ]) #Matrix with the position of the K closest values
  
  
  for(i in 1:nrow(K_matrix)){
    for(j in 1:ncol(K_matrix)){
      K_matrix[i,j] = train_spam[K_matrix[i,j]] # replace the five K values with the values of 1,0 in the column of train 
    }
  }
  if(length(prob)==1){ # doing the mean of the K closest values and replace it for 1 or 0
    if(K == 1){
      K_matrix = apply(K_matrix,1, function (i){ifelse(i>prob,1,0)})
    }
    else{
      K_matrix = apply(K_matrix,1, function (i){ifelse(mean(i)>prob,1,0)})
    }
    
    if(identical(data,mytest)){ #If the data introduced and the data to predict are the same, then table on train
      
      result = table(K_matrix,train_spam) #Confusion matrix
      misclassification_rate = (result[1,2]+result[2,1])/sum(result) #missclassification rate
      
      res<- list()
      res[["missclassification_rate"]]<-misclassification_rate
      res[["result"]]<-result
      return(res)
      
    }else{ #If the data introduced and the data to predict are the same, then table on test
      result = table(K_matrix,test_spam) #Confusion matrix
      misclassification_rate = (result[1,2]+result[2,1])/sum(result) #missclassification rate
      res<- list()
      res[["missclassification_rate"]]<-misclassification_rate
      res[["result"]]<-result
      return(res)
    }
    
  }else{
    misclassification_rate <- integer(length(prob)) ## Setting values for the confusion matrix
    confusion_matrix <-list()
    
    FP<- integer(length(prob))
    TP<- integer(length(prob))
    FN<- integer(length(prob))
    TN<- integer(length(prob))
    N_pos <- integer(length(prob))
    N_neg <- integer(length(prob))
    
    
    for(i in 1:length(prob)){
      if(K == 1){
        ## it is just a data frame of one column so it is ony needed to change probabilities by 1 or 0
        Kmatrix <- apply(K_matrix,1, function(a){ifelse(a>prob[i],1,0)})
      }
      else{
        Kmatrix <- apply(K_matrix,1, function(a){ifelse(mean(a)>prob[i],1,0)})
      }
    confusion_matrix[[i]]<-table(Kmatrix,test_spam)
    result <- table(Kmatrix,test_spam)
    misclassification_rate[i] <-(result[1,2]+result[2,1])/sum(result)
    
    
    FP[i]<- result[2,1]
    TP[i]<- result[2,2]
    FN[i]<- result[1,2]
    TN[i]<- result[1,1]
    N_pos[i]<- result[2,2]+ result[1,2]
    N_neg[i]<- result[1,1]+ result[2,1]
    
    }
    sensitivity <-TP/N_pos
    FPR<- (FP/N_neg)
    specificity <- 1- FP/N_neg
    
    #creating a list to store the results
    res<- list()
    res[["missclassification_rate"]]<-misclassification_rate
    res[["confusion matrix"]]<-confusion_matrix
    res[["sensitivity"]] <- sensitivity
    res[["FPR"]]<- FPR
    res[["specificity"]]<-specificity
    
    return(res)  
}}


###3
  
hello = knearest( data= train, K =5, newdata= test, prob = 0.5)
knearest( data= train, K =5, newdata= train, prob = 0.5)



###4
knearest( data= train, K =1, newdata= test, prob = 0.5)
knearest( data= train, K =1, newdata= train, prob = 0.5)


# For k = 1, the number of missclassifications increases from 40.1% when K = 3 to 42.9%. 

###5

library(kknn)

mykknn_fittedValues <- kknn(Spam~., train =train_complete, test = test_complete, k = 5)$fitted.values

mykknfunction <- function(fittedvalues=mykknn_fittedValues, prob=0.5){
  if(length(prob)==1){
    # We just get a vector of probabilities that need to be changed to 1 if> prob, else 0
    mychoiceofprob <- ifelse(fittedvalues > prob, 1, 0)
    
    confusion_matrix<- table(mychoiceofprob,test_spam)

    misclassification_rate <-(confusion_matrix[1,2]+confusion_matrix[2,1])/sum(confusion_matrix)
    
    #creating a list to store the results
    
    res<- list()
    res[["missclassification_rate"]]<-misclassification_rate
    res[["confusion_matrix"]]<-confusion_matrix
    return(res)
    
    
  }else{
    misclassification_rate <- integer(length(prob)) 
    # The probability is a vector, so it needs to be stored n a vector for each number
    FP<- integer(length(prob))
    TP<- integer(length(prob))
    FN<- integer(length(prob))
    TN<- integer(length(prob))
    N_pos <- integer(length(prob))
    N_neg <- integer(length(prob))
    confusion_matrix<- list()
    for(i in 1:length(prob)){
      
      mychoiceofprob <- ifelse(fittedvalues > prob[i], 1, 0)
      
      
      confusion_matrix[[i]]<- as.matrix(table(mychoiceofprob,test_spam))
      simpleconfusion <- table(mychoiceofprob,test_spam)

      misclassification_rate[i] <-(simpleconfusion[1,2]+simpleconfusion[2,1])/sum(simpleconfusion)
      FP[i]<- simpleconfusion[2,1]
      TP[i]<- simpleconfusion[2,2]
      FN[i]<- simpleconfusion[1,2]
      TN[i]<- simpleconfusion[1,1]
      N_pos[i]<- simpleconfusion[2,2]+ simpleconfusion[1,2]
      N_neg[i]<- simpleconfusion[1,1]+ simpleconfusion[2,1]
    }
    sensitivity <-TP/N_pos
    FPR<- (FP/N_neg)
    specificity <- 1- (FP/N_neg)
    
    
    res<- list()
    res[["missclassification_rate"]]<-misclassification_rate
    res[["confusion matrix"]]<-confusion_matrix
    res[["sensitivity"]] <- sensitivity
    res[["FPR"]]<-FPR
    res[["specificity"]]<-specificity
    
  return(res)
  }
}



mykknfunction(mykknn_fittedValues, 0.5)



###6

mypi<- seq(0.05,0.95, 0.05)
#Storing the values to plot

Myknn_sensitivity<-mykknfunction(mykknn_fittedValues, prob =mypi)$sensitivity
Myknn_specificity<-mykknfunction(mykknn_fittedValues, prob =mypi)$specificity
Myknn_FPR<-mykknfunction(mykknn_fittedValues, prob =mypi)$FPR
Myfunction_sensitivity<-knearest(data = train, K=5, newdata =test, prob =mypi)$sensitivity
Myfunction_specificity<-knearest(data = train, K=5, newdata =test, prob =mypi)$specificity
Myfunction_FPR<-knearest(data = train, K=5, newdata =test, prob =mypi)$FPR


#False Positive Rates (FPR)
#Probability of false alarm: system alarms (1) when nothing happens (true=0)

library(ggplot2)


myPlot<- function(yourfunction_sens,yourfunction_fpr , packagefunction_sens,packagefunction_fpr, title = "ROC Curve"){
  data <- data.frame(my_sens = yourfunction_sens, #Creating data frame for the plot with the vectors entered in the function
                     my_fpr = yourfunction_fpr, 
                     kknn_sens = packagefunction_sens,
                     kknn_fpr = packagefunction_fpr)
  
  result<-ggplot2::ggplot(data = data)+ 
            geom_line(aes( x = my_fpr, y = my_sens, color = "knearest"))+
            geom_line(aes(x = kknn_fpr, y = kknn_sens, color = "kknn"))+ 
            xlab("FPR" )+ylab("TPR")+ ggtitle(title)+ geom_abline(intercept =0,slope =1)+
            xlim(0,1)+ylim(0,1)+theme_bw()
  return(result)
  }

ROCplot<-myPlot(Myfunction_sensitivity,Myfunction_FPR,Myknn_sensitivity, Myknn_FPR)




##### Assignment 2

##1.Importing data
set.seed(12345)  
library(readxl)
data<-read_excel("machines.xlsx")

##2. The distribution type of x is an exponential distribution
logLikelihood<- function(theta, x){
  
  result<-integer(0)
  ##it is the loglikelihood(product and logarithms)
  for(i in 1:length(theta)){ 
    result[i]<- nrow(x)*log(theta[i])-theta[i]*sum(x)
  }
  return(result)
}

theta1<-seq(0,4,0.01)   #theta between 0 and 4 by 0.01 
myloglikelihood<-logLikelihood(theta = theta1,x = data)

library(ggplot2)
qplot(theta1,myloglikelihood, geom="line")  #plotting with ggplot2 
my_theta_loglikelihood<-data.frame(theta1,myloglikelihood)  #find the best theta for the data created 
mybesttheta<- my_theta_loglikelihood[order(my_theta_loglikelihood$myloglikelihood, decreasing = TRUE),][1,]
print(mybesttheta)
#According to the plot it looks like 1, but by checking on the data.frame created the best theta is 1.13 for the max loglikelihood of -42.2948

##3.

myfirst_6_observations<-as.matrix(data[1:6,]) # the first six first observations of the data frame

myloglikelihood_6_obs<-logLikelihood(theta1, myfirst_6_observations)  #calling the function to the six obs 

mydatatoplot<- data.frame(theta1 = theta1, all_data= myloglikelihood, six_obs = myloglikelihood_6_obs )
myplot_activity3 <- ggplot(mydatatoplot)+
  geom_line(aes(x=theta1, y= all_data, color = "All data"))+
  geom_line(aes(x=theta1, y= six_obs, color = "First 6 observations"))+
  ggtitle("Log likelihood plot")+
  ylab("Likelihood")+ theme_bw()

# The first 6 observations give a better likelihood. 
# However, having only six observations is not enough to get a reliable good likelihood so that the all data likelihood is more reliable


##4.
bayes<- function(theta, x, lambda=10){ # lambda default 10 
  prior<-numeric(length(theta)) #empty vector to fill with new values 
  for ( i in 1:length(theta)){   
    prior[i]<-log(lambda * exp(-lambda*theta[i]))  # log of prior 
    
  }
  loglikke<-logLikelihood(theta,x) # calling loglike function above
  io<-prior+loglikke  # adding prior and loglike according to ln rules
  return(io)
}
test<-bayes(theta=theta1, x= data, lambda = 10)
qplot(theta1,test, geom="line")  #plotting    

my_theta_loglikelihood2<-data.frame(theta1,test)
mybesttheta2<- my_theta_loglikelihood2[order(my_theta_loglikelihood2$test, decreasing = TRUE),][1,]
print(mybesttheta2)

#Previous best theta was 1.13 for the max loglikelihood of -42.2948
#New theta is 0.91 for the max loglikelihood of -50.10905. 


##5.

randomexp<-rexp(50, rate=mybesttheta[[1]]) #random from exp distribution with the best theta from 2.2 
randomexp<- as.data.frame(randomexp) 
colnames(randomexp)<- "Length" #Setting the same name as the original data to get it together

binding<-rbind(randomexp,data)  
Data<- c(rep("generated data",50), rep( "original data",48))
Data<- as.data.frame(Data)
mydataframe <- data.frame(binding$Length, Data) # Setting the data frame for the plot with data and if this data is original or generated
hist_plot<- ggplot(mydataframe, aes(binding$Length, fill = Data))+
  geom_histogram(alpha = 0.4, bins=35, position = 'identity')+
  labs(title= "Exponential data", 
       x = "Life length of machines", 
       y = "Count")+theme_bw()+
  theme(panel.grid.major = element_blank())



