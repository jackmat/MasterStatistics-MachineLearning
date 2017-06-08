##Assignment 1

##2

#2.1
set.seed(1234567890)
data<- read.csv2("C:/Users/M/Desktop/Statistics and Data Mining Master/Semester 1/Second part of the semester/intro to machine learning/lab2block2/bodyfatregression.csv")
library(tree)

n<-dim(data)[1]
id<-sample(1:n, floor(n*2/3)) 
train=data[id,] 
test= data[-id,]


random_split_data<- function(data, Nfolds){
  
  set.seed(12345)
  n <- dim(data)[1]
  sample_numbers<-sample(1:n)
  
  ##indexes rows for train
  randomsplitvector<-split(sample_numbers, ceiling(seq_along(1:n)/(n/Nfolds)))
  
  # indexes rows complementary of train
  randomsplitvector_compl<- list()
  for(i in 1:Nfolds){
    randomsplitvector_compl[[i]]<- sample_numbers[!sample_numbers %in% randomsplitvector[[i]]]
    
  }
  res<-list()
  res[["train"]]<-randomsplitvector_compl
  res[["test"]]<- randomsplitvector
  return(res)
  
}



bootstrap<- function(data, B, testdata, method, Nfolds){
  #inititializing data
  if(method == "holdout"){
    MSE<- integer(B)
    for(i in 1:B){
      n<- dim(data)[1]
      id <- sample(1:n, replace = TRUE)
      in_train<-data[id,]
      
      fit <- tree(formula = Bodyfat_percent~Waist_cm+Weight_kg, in_train)
      my_pred<- predict(fit, newdata = testdata)
      residuals<- testdata$Bodyfat_percent-my_pred
      
      MSE[i]<- (sum((residuals)**2))/nrow(testdata)
    }
    
    res<- list()
    res[["MSE"]]<- MSE
    
    
    return(res)
    
    }
  
  if(method == "CV"){
    randomsplitvector_train<- random_split_data(data = data, Nfolds = Nfolds)$train
    randomsplitvector_test<- random_split_data(data = data, Nfolds = Nfolds)$test
    ###Partitioning data with its real data
    datapartitioned_train<-list()
    datapartitioned_test<-list()   
    
    
    MSE<- integer(B)
    
    
    for(k in 1:B){
      n<- dim(data)[1]
      id <- sample(1:n, replace = TRUE)
      datasample<-data[id,]
      for(i in 1:Nfolds){
        
        datapartitioned_train[[i]]<- datasample[randomsplitvector_train[[i]],] 
        
      }
      
      
      for(i in 1:Nfolds){
        
        datapartitioned_test[[i]]<- datasample[randomsplitvector_test[[i]],] 
        
      }
      #Setting differents numbers using sample to partition data in an outside function
      
      
      ##Calculating residuals and chossing the smaller one of the Nfolds

      for(j in 1:Nfolds){
        fit <- tree(formula = Bodyfat_percent~Waist_cm+Weight_kg, datapartitioned_train[[j]])
        my_pred<- predict(fit, newdata = datapartitioned_test[[j]])
        residuals<- datapartitioned_test[[j]]$Bodyfat_percent-my_pred
        MSE[k]<- (sum((residuals)**2))/length(my_pred)
        }
      
    }
    return(MSE)}
}
bootstrap_holdout<-bootstrap(data= train, B=1000, testdata = test, method = "holdout")
mean(bootstrap_holdout$Tree)
bootstrap_CV<-bootstrap(data= data, B=1000, testdata = data, method = "CV", Nfolds = 3)

my_mean_CV<-mean(bootstrap_CV)

##average error of your models cannot be higher than its mean so that
upper_bound_holdout<-mean(bootstrap_holdout)

hist(bootstrap_holdout)


#2.2

fit <- tree(formula = Bodyfat_percent~Waist_cm+Weight_kg, data= train)
N_tree<- cv.tree(fit, K = 3)

best_size <- N_tree$size[which.min(N_tree$dev)]  


#2.3

TreeBagRegression<- function(data, B){
  #inititializing data

    fit<- list()
    for(i in 1:B){
      n<- dim(data)[1]
      id <- sample(1:n, replace = TRUE)
      in_train<-data[id,]
      
      fit[[i]] <- tree(formula = Bodyfat_percent~Waist_cm+Weight_kg, in_train)
      
    }
    return(fit)
}

TreeBagRegression(data = data, B=10)


####4

data2<- read.csv2("C:/Users/M/Desktop/Statistics and Data Mining Master/Semester 1/Second part of the semester/intro to machine learning/lab2block2/spambase.csv")
data2[["Spam"]]<- as.factor(data2[["Spam"]]) #necessary for data to work
names(data2)
n<-dim(data2)[1]
set.seed(1234567890)
id <- sample(1:n, floor(n*2/3))
train2 <- data2[id,]
test2 <-  data2[-id,]

#install.packages("mboost")
library(mboost)
library(tree)
library(party)
library(randomForest)
library(ggplot2)
##Drawing sequence of number of trees
myseq<- seq(10,100,10)
adaboost_error<- integer(length(myseq))
RF_error<- integer(length(myseq))
for(size in 1:length(myseq)){
  #Adaboost formula
  ada_boost<-blackboost(formula = Spam~., 
                      data = train2, 
                      family = AdaExp(), 
                      control = boost_control(mstop = myseq[size]))
  #Random forest usage
  RF<- randomForest::randomForest(formula = Spam~., data =train2, ntree = myseq[size]) 

  #Predicting and getting errors for adaboost              
  ada_pred<-predict(ada_boost, newdata = test2, type ="class")
  conf_ada <-table(test2$Spam, ada_pred)
  adaboost_error[size] <- conf_ada[1,2]+conf_ada[2,1]
  
  #Predicting and getting errors for RF
  RF_pred  <-predict(RF, newdata = test2, type ="class")
  conf_RF <-table(test2$Spam, RF_pred)
  RF_error[size] <- conf_RF[1,2]+conf_RF[2,1]


}
result <- data.frame(adaboost_error= adaboost_error, RF_error = RF_error)

ggplot2::ggplot(result)+
  geom_line(aes(x = seq(10,100,10), adaboost_error, color = "adaboost"))+ 
  geom_line(aes(x = seq(10,100,10), RF_error, color = "RF_error"))+
  xlab("number of trees considered from 10-100 by 10")+ ylab("error prediction")+
  theme_bw()



tre<-tree(formula = Bodyfat_percent~Waist_cm+Weight_kg, train)
tre$frame

set.seed(1234567890)
max_it <- 100 # max number of EM iterations
min_change <- 0.1 # min change in log likelihood between two consecutive EM iterations
N=1000 # number of training points
D=10 # number of dimensions
x <- matrix(nrow=N, ncol=D) # training data
true_pi <- vector(length = 3) # true mixing coefficients
true_mu <- matrix(nrow=3, ncol=D) # true conditional distributions
true_pi=c(1/3, 1/3, 1/3)
true_mu[1,]=c(0.5,0.6,0.4,0.7,0.3,0.8,0.2,0.9,0.1,1)
true_mu[2,]=c(0.5,0.4,0.6,0.3,0.7,0.2,0.8,0.1,0.9,0)
true_mu[3,]=c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5)
plot(true_mu[1,], type="o", col="blue", ylim=c(0,1))
points(true_mu[2,], type="o", col="red")
points(true_mu[3,], type="o", col="green")
# Producing the training data
for(n in 1:N) {
  k <- sample(1:3,1,prob=true_pi)
  for(d in 1:D) {
    x[n,d] <- rbinom(1,1,true_mu[k,d])
  }
}
K=3 # number of guessed components
z <- matrix(nrow=N, ncol=K) # fractional component assignments
pi <- vector(length = K) # mixing coefficients
mu <- matrix(nrow=K, ncol=D) # conditional distributions
llik <- vector(length = max_it) # log likelihood of the EM iterations
# Random initialization of the paramters
pi <- runif(K,0.49,0.51)
pi <- pi / sum(pi)
for(k in 1:K) {
  mu[k,] <- runif(D,0.49,0.51)
}
pi
mu


for(it in 1:max_it) {
  plot(mu[1,], type="o", col="blue", ylim=c(0,1))
  points(mu[2,], type="o", col="red")
  points(mu[3,], type="o", col="green")
  #points(mu[4,], type="o", col="yellow")
  #Sys.sleep(0.5)
  
  
  ###E-Step
  E_step<- function(x,mu,pi){
    
    #the number of diision output
    ##computing the likelihoods for the  = 3 cases different cases
    ##new way
    probx_mu<- integer(N)
    
    for(thousand in 1:N) {
      prob<- 0
      for(three in 1:K) {
        prob <- prob + (pi[three] * prod((mu[three,]^x[thousand,]) * (1-mu[three,])^(1-x[thousand,])))
        ########################product of ######pi_k*##mu**x*(1-mu)**(1-x)
      }
      probx_mu[thousand]<- prob
    }
    
    # Calculating the matrix Z
    for (thousand in 1:N) {
      for (three in 1:K) {
        z[thousand,three] <-(pi[three]*(prod((mu[three,]^x[thousand,])*(1-mu[three,])^(1-x[thousand,]))))/probx_mu[thousand]
      }
    }
    return(z) 
  }
  
  
  
  z<-E_step(x=x,mu=mu,pi=pi)
  
  count<-1
  
  #2-llik[it] = the sum log of my z1 total
  newLike<-matrix(0,nrow=1000,ncol=3)
  for (n in 1:N) {
    for (k in 1:K) {
      newLike[n,k] <- (pi[k]*(prod((mu[k,]^x[n,]) * (1-mu[k,])^(1-x[n,]))))
    }
  }
  
  llik[it]<-sum(log(rowSums(newLike)))
  
  ##Breaking loop
  if(count>1){
    if(abs(llik[it])/abs(llik[it-1])<0.1){
      stop(return(llik[it]))
    }
  }
  
  count<-count+1
  
  
  mstep<-function(x,z){
    #new pi
    pi<-apply(z,2,mean)
    
    #new mu
    den<-apply(z,2,sum)
    
    for(ten in 1:D){
      for(three in 1:K){
        
        up_fraction <-sum(x[,ten]*z[,three])
        mu[three,ten]<-up_fraction/den[three]
      }}
    res<-list(mu=mu, pi=pi)
    return(res)
  }
  mu<-mstep(x=x,z=z)$mu
  pi<-mstep(x=x,z=z)$pi
  
  cat("iteration: ", it, "log likelihood: ", llik[it], "\n")
  flush.console()
  # Stop if the lok likelihood has not changed significantly
  # Your code here
  #M-step: ML parameter estimation from the data and fractional component assignments
  # Your code here
}
pi
mu
z
plot(llik[1:it], type="o")


##



























