###Assignment 1

#importing data

data0<- read.csv2("C:/Users/M/Desktop/Statistics and Data Mining Master/Semester 1/Second part of the semester/intro to machine learning/lab3 block 2/data.csv", fileEncoding ="latin1"  )
#1

##dividing data
data<-data0
n<- dim(data)[1]


#install.packages("pamr")
library(pamr)
data <- as.data.frame(data)
data$Conference=as.factor(data$Conference)

set.seed(12345) 
id<- sample(1:n, 0.7*n)
train<- data[id,]
test<-data[-id,]
dim(test)
rownames(train) <- 1:nrow(train)

x <- t(train[,-ncol(train)])
y <- train$Conference

x_t <- t(test[,-ncol(test)])
y_t <- test$Conference
mydata <-list(x=x,y=as.factor(y),geneid=as.character(1:nrow(x)), genenames=rownames(x))
mydatatest <-list(x=x_t,y=as.factor(y_t),geneid=as.character(1:nrow(x_t)), genenames=rownames(x_t))

model <- pamr.train(mydata,threshold=seq(0,4, 0.1))


##cross-validation for the model 
cvmodel<- pamr.cv(model,mydata)
print(cvmodel)

##chossing threshold which error is minimized
my_threshold<- cvmodel$threshold[which.min(cvmodel$error)]
pamr.plotcen(model, mydata, threshold=my_threshold)




#Necessary to expand my margins on the right side
pamr.plotcv(cvmodel)

bestmodel<- pamr.train(mydata,threshold=my_threshold)
mypred<-pamr.predict(fit = bestmodel, type = "centroid", newx = as.matrix(test), threshold = my_threshold)
pamr.confusion(fit=bestmodel, threshold=my_threshold , extra=TRUE)

a<- pamr.listgenes(bestmodel,mydata,threshold= my_threshold)
cat( paste( colnames(data)[as.numeric(a[,1])][1:10], collapse='\n' ))


#2
library(glmnet)
x_not_t <- (train[,-ncol(train)])
x_t_not_t<- (test[,-ncol(test)])
#Checking best lambda by cv  
mycv<- cv.glmnet(x = as.matrix(x_not_t), y = as.factor(y), family = "binomial", alpha = 0.5)
plot(mycv)
lambda_min<- mycv$lambda.min

bestcv_model<- glmnet(x = as.matrix(x_not_t), y = y, family = "binomial", alpha = 0.5, lambda = lambda_min)
coef(bestcv_model)
elastic_pred<-predict(bestcv_model, newx = as.matrix(x_t_not_t), type ="class")
table(elastic_pred= elastic_pred, Observed_values = test$Conference )

#install.packages("kernlab")
library(kernlab)


K <- as.kernelMatrix(crossprod(t(x)))
res<- kpca(K, kernel = "vanilladot")
barplot(res@eig)
plot(res@rotated[,1], res@rotated[,2], xlab="PC1", ylab="PC2")

kernel_pred <-predict(res, newx = as.matrix(x_t_not_t), type ="class")
#3
namesdata<-names(data)
pvalue<- integer(0)

a<-t.test(formula = Conference~X000euro, data = data, alternative="two.sided")

for(i in 1:(length(namesdata)-1)){
  formula<-  as.formula(paste("Conference ~", namesdata[i]))
  pvalue[i]<-t.test(formula = formula, data = data, alternative="two.sided")$p.value 
  
}

##Asignment 2

set.seed(1234567890)
spam <- read.csv2("C:/Users/M/Desktop/Statistics and Data Mining Master/Semester 1/Second part of the semester/intro to machine learning/lab3 block 2/spambase.csv")
ind <- sample(1:nrow(spam))
spam <- spam[ind,c(1:48,58)]

gaussian_h <- 1
beta  <-  c(0,-0.05,0,-0.05) # Your value here
M   <-  c(500, 500, 20, 20) # Your value here
N   <- 500 # number of training points




mySVMfunction <- function(M, beta){
  
  b<-0
  
  mydata <- model.matrix(Spam~., data = spam)
  mydata <- mydata[,-1] # taking out the last column of proof
  spam[,"Spam"]<-2*spam[ncol(spam)]-1 # last colum from spam or not by one or 0
  myspamvect <- spam[,all.vars(Spam~.)[1]]
  
  
  ##my function to calculate the euclidean distance between two vectors
  distancevectors<- function(vector1, vector2){
    if(length(vector1)==length(vector2)){
      myresult<- sqrt(sum((vector1-vector2)**2))
      return(myresult)
    }
    else{(stop("vectors are not of the same length"))
    }
  }
  
  #gaussian kernel function just to pass the eucliedean distance
  gaussian_k <- function(x, h = gaussian_h) { # Gaussian kernel h = 1
    gaussiandist<- exp(-(x**2/(2*h^2)))   #h = 2*var(x), where variance = 1    
    return(gaussiandist)
  }
  
  ##calculating y
  SVM <- function(sv,i) { # SVM on point i with support vectors sv
    
    vector_dist<- as.matrix(dist(mydata[c(i,sv),]))[-1,1]#euclidean distance
    kernel_distance<-gaussian_k(x=vector_dist) #one vector distance point between both data
    y<-   1*myspamvect[sv]*kernel_distance+b #a = 1
    return(sum(y))
    
  }
  
  errors <- 1
  errorrate <- vector(length = N)
  errorrate[1] <- 1
  sv <- c(1)
  
  for(i in 2:N) {
    # Your code here
    y_i<-SVM(sv = sv, i = i)
    
    if(myspamvect[i]*y_i<0){##if t_i*y<= 0, then error
      errors<- errors+1
      
      
      if(myspamvect[i]*y_i<beta){##if t_i*y<= Beta, then 
        sv[length(sv) + 1] <- i 
        
        if(length(sv) > M){
          res_temp <- lapply(seq_along(1:length(sv)), function(j){
            svm_temp <- SVM(sv = sv, i = sv[j])
            itself <- 1*myspamvect[sv[j]]*
              gaussian_k(x = dist(mydata[c(sv[j],sv[j]),]))
            res_out <- myspamvect[sv[j]]*(svm_temp-itself)
            return(res_out)
            
          })
          sv <- sv[-which.max(res_temp)]
        }
      }
    }
    errorrate[i] <- errors / i
  }
  return(list(errorrate,sv))
}


res1 <- mySVMfunction(M = 500, beta =  0)
res2 <- mySVMfunction(M = 500, beta = -0.05)
res3 <- mySVMfunction(M = 20, beta = 0)
res4 <- mySVMfunction(M = 20, beta = -0.05)

sv1<-res1[[2]]
sv2<-res2[[2]]  
sv3<-res3[[2]]
sv4<-res4[[2]]

res_1<-res1[[1]]
res_2<-res2[[1]]  
res_3<-res3[[1]]
res_4<-res4[[1]]

res_1==res_2
myframe<- data.frame(res_1, res_2, res_3, res_4)
library(ggplot2)
ggplot2::ggplot(myframe)+
  geom_line(aes(x= 1:N, y = res_1, color = "M = 500, beta = 0"))+
  geom_line(aes(x= 1:N, y = res_2, color = "M = 500, beta = -0.05"))+
  geom_line(aes(x= 1:N, y = res_3, color = "M = 20, beta = 0"))+
  geom_line(aes(x= 1:N, y = res_4, color = "M = 20, beta = -0.05"))+
  ylab("error rates")+  theme_bw()



plot(res_1, x = 1:N, ylim=c(0,1), type="l", col = "red", xlab = "N",
     ylab = "Error Rates")
lines(res_2, x = 1:N, ylim=c(0.4,1), type="l", col = "blue")
lines(res_3, x = 1:N, ylim=c(0.4,1), type="l", col = "green")
lines(res_4, x = 1:N, ylim=c(0.4,1), type="l", col = "black")
legend("topright", c("M = 500, beta = 0","M = 500, beta = -0.05",
                     "M = 20, beta = 0","M = 20, beta = -0.05"),
       lty = c(1,1),
       col = c("red","blue","green","black"))




