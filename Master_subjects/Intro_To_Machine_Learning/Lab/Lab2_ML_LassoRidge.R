##R script activity 1
data(swiss)


##FUnction that gives all the combinatorial of the data entered
all_beta_subsets<-function(xvar,n){
  
  binaryList <- rep(list(0:1), 5)
  
  exgrid<-expand.grid(binaryList)
  
  expand<-rowSums(exgrid)!=0 # Eliminating the line of all 0
  es<-exgrid[expand,]
  logisk<-es==1
  namn<-colnames(xvar)
  betasubsets<-list()
  for (i in 1:nrow(logisk)){
    betasubsets[i]<-paste( namn[logisk[i,]], collapse= " + ")
    
  }
  res<- list()
  res[["betasubsets"]]<-betasubsets
  res[["number of variables per model"]]<- rowSums(es)
  return(res)
}


## Function that includes y a list
includeNameInList<-function(name, lista){
  funct_list<-lista
  for (i in 1:length(funct_list)){
    funct_list[[i]]<- paste(name, funct_list[[i]], sep = " + ")
  }
  
  return(funct_list)  
}

##It random splits data
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

random_split_data(data = swiss, Nfolds = 5)





lmFold <- function(x=swiss[,2:6], y=swiss[,1], Nfolds=5){
  
  set.seed(12345)
  data<- cbind(y=y,x)  
  ##All combinations for data
  mybeta_subsets<- all_beta_subsets(x,Nfolds)$betasubsets
  vector_comb_with_pred<- includeNameInList("y", mybeta_subsets)
  
  #Setting differents numbers using sample to partition data in an outside function
  
  randomsplitvector_train<- random_split_data(data = data, Nfolds = 5)$train
  randomsplitvector_test<- random_split_data(data = data, Nfolds = 5)$test
  ###Partitioning data with its real data
  datapartitioned_train<-list()
  datapartitioned_test<-list()   
  for(i in 1:Nfolds){
    
    datapartitioned_train[[i]]<- data[randomsplitvector_train[[i]],] 
    
  }
  
  
  for(i in 1:Nfolds){
    
    datapartitioned_test[[i]]<- data[randomsplitvector_test[[i]],] 
    
    }

    res <- list() 
     ## getting the string to pass in the model matrix as part of the data model 
      for( j in 1:length(vector_comb_with_pred)){  
        str=strsplit(vector_comb_with_pred[[j]],split = " [+] ")
        
        for(i in 1:Nfolds){
          for(name in str){
          #Intialization of matrixes for train and test
        
        x_matrix_train <- model.matrix(y~. - y, datapartitioned_train[[i]][name])
        y_vector_train <- datapartitioned_train[[i]][[1]]
        x_matrix_test <- model.matrix(y~. - y, datapartitioned_test[[i]][name])
        y_vector_test <- datapartitioned_test[[i]][[1]]
          }
        
        #Calculations of the coefficients
        res[["coefficients train"]] <-  solve((t(x_matrix_train)%*%x_matrix_train))%*%t(x_matrix_train)%*%y_vector_train
        res[["fitted.values"]] <- x_matrix_test%*%res[["coefficients train"]]
        res[["residuals"]] <- y_vector_test - res[["fitted.values"]]
        res[["sum of residuals squared"]][i]<-sum(res[["residuals"]]**2)
        
        
        
        
        #Transform matrix object to vector format
        res[["coefficients train"]] <- as.vector(res[["coefficients train"]])
        names(res[["coefficients train"]]) <- colnames(x_matrix_train)
        
        res[["fitted.values"]] <- as.vector(res[["fitted.values"]])
        
        res[["residuals"]] <- as.vector(res[["residuals"]])
        
        res[["sum of residuals squared"]][i]<-as.vector(res[["sum of residuals squared"]][i])
        
        
          
      
        }
        ##Calculation of the MSE
        res[["MSE"]][j]<- sum(res[["sum of residuals squared"]])/dim(data)[1]
      }
      #Class assignment
    class(res) <- "lmFold"
    
    #Output
    
    return(res[["MSE"]])
}

set.seed(12345)




##2

#Storing result
result<-lmFold(x = swiss[,2:6],y= swiss[,1], Nfolds = 5)
##storing numbers of variables in the combinatorial analysis
Number_Variables_per_model <- all_beta_subsets(swiss[,2:6], 5)$`number of variables per model`
##Plotting
library(ggplot2)
myframe<- data.frame(Number_Variables_per_model, result)
ggplot2::ggplot(myframe) + 
        geom_point(aes(x = Number_Variables_per_model, y = result))+
        ylab("CV score")

indexminmse<-which.min(myframe$result)
minmse<-min(myframe$result)
regressionresultmse<- all_beta_subsets(swiss[,2:6], 5)$betasubsets[indexminmse]


##Results of the best model with the variables
minresultmse<-data.frame(indexminmse, minmse, regressionresultmse)

  


############# Assignment 2
mydir<- 
  setwd("C:/Users/M/Desktop/Statistics and Data Mining Master/Semester 1
        /Second part of the semester/intro to machine learning/lab2")

mydir
library(readxl)

mydata<- read_excel("tecator.xlsx")

# 1)PLot moisture against protein
myPlot<- function(x, y){
  datfram<- data.frame(x = x,y = y)
  library(ggplot2)
  ggplot2::ggplot(datfram)+
    geom_point(aes(x = x, y = y))+
    xlab(deparse(substitute(x)))+ ylab(deparse(substitute(y)))+
    theme_dark()
}

myPlot(x = mydata$Moisture, y = mydata$Protein) 


# It looks like a linear regression works properly


##2 &3 )

##Partitioning data
n = dim(mydata)[1]
set.seed(12345)
id = sample(1:n, floor(n*0.5))
train = mydata[id,]
test= mydata[-id,] 

## Setting the model for different lm regressions  
Protein <- list()
Protein[["Protein1"]]<-lm(Moisture~Protein, data = train)
Protein[["Protein2"]]<-lm(Moisture~Protein+ I(Protein**2), data = train)
Protein[["Protein3"]]<-
  lm(Moisture~Protein+ I(Protein**2)+I(Protein**3), data = train)
Protein[["Protein4"]]<-
  lm(Moisture~Protein+ I(Protein**2)+I(Protein**3)+I(Protein**4), data = train)
Protein[["Protein5"]]<-lm(Moisture~Protein+ I(Protein**2)+I(Protein**3)+
                            I(Protein**4)+I(Protein**5), data = train)
Protein[["Protein6"]]<-lm(Moisture~Protein+ I(Protein**2)+I(Protein**3)+
                            I(Protein**4)+I(Protein**5)+I(Protein**6), data = train)

#Creating predctions for my  test data
myprediction_test<-list()
for(i in 1:length(Protein)){
  myprediction_test[[i]]<- predict(Protein[[i]], newdata= test)
} 

##Creating a data frame of all the test predictions data to pass to calculate the MSE
myprediction_test<- as.data.frame(myprediction_test)
colnames(myprediction_test)<-c(1:6)

#Creating predctions for my train data
myprediction_train<-list()
for(i in 1:length(Protein)){
  myprediction_train[[i]]<- predict(Protein[[i]], newdata= train)
} 


##Creating a data frame of all the train predictions data to pass to calculate the MSE
myprediction_train<- as.data.frame(myprediction_train)
colnames(myprediction_train)<-c(1:6)



#install.packages("hydroGOF")
library(hydroGOF)
## Getting the different MSE values for both train and test
msevector_test<-integer(length(myprediction_test))

for( i in 1:length(myprediction_test)){
  msevector_test[i] <- mse(myprediction_test[[i]], test$Moisture)
}

msevector_train<-integer(length(myprediction_train))

for( i in 1:length(myprediction_train)){
  msevector_train[i] <- mse(myprediction_train[[i]], train$Moisture)
}

# plotting both MSE vectors

myplot2<- function(train,test){
  thedataframe <- data.frame(x = 1:length(train),train = train, test = test) 
  plot<-ggplot2::ggplot(thedataframe)+
    geom_line(aes(x = x, y = train, color = "train"))+
    geom_line(aes(x = x, y = test, color = " test"))+
    ylab("values of MSE")+
    theme_bw()
  return(plot)
}

myplot2(train =msevector_train, test = msevector_test)


##The best predictor for this case is the base one

##4
library(MASS)
fitlm <- lm(Fat~.- Moisture - Protein - Sample, data = mydata)
stepaicResultlm<- stepAIC(fitlm, direction ="both") 

coefficientslm<- stepaicResultlm$coefficients #Sroring lmcoefficients from StepAIC
length(coefficientslm) ## 64 variables were selected

stepaicResultlm$fitted.values

myPlot(x = mydata$Fat, y  = stepaicResultlm$fitted.values)



##5 & 6
#install.packages("glmnet")
library(glmnet)

## normalizing data and using the data to regress on 0 = Ridge, 1 = LASSO
covariates<- scale(mydata[,2:101])
response <- scale(mydata$Fat)
fitridge <- 
  glmnet(x = as.matrix(covariates), y = response, alpha = 0, family = "gaussian")
plot(fitridge, xvar = "lambda", label = TRUE)

fitlasso <-
  glmnet(x = as.matrix(covariates), y = response, alpha = 1, family = "gaussian")
plot(fitlasso, xvar = "lambda", label = TRUE)

##7
##Using crossvalidation formulas in glmnet and plotting
bestfitlasso <-
  cv.glmnet(x = as.matrix(covariates), y = response, alpha = 1, family = "gaussian")
coefbestfitlasso <- coef(bestfitlasso, s= "lambda.min", extract = TRUE)
coefbestfitlasso_result<-coefbestfitlasso[coefbestfitlasso[,1]!=0,]
length(coefbestfitlasso_result)#14- 1 as intercept
plot(bestfitlasso)





