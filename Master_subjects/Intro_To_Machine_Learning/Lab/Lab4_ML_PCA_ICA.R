####Assignment 1 

##1

##Reading State File
data<- read.csv2("C:/Users/M/Desktop/Statistics and Data Mining Master/Semester 1/Second part of the semester/intro to machine learning/lab4/State.csv", sep =";")

#Information about data
##  MET: Percentage of population living in standard metropolitan areas
##  EX: Per capita state and local public expenditures ($)

##ordeding data by MET
ordered_data<- data[order(data$MET),]
##Plotting EX versus MET
library(ggplot2)
ggplot2::ggplot(ordered_data)+
  geom_point(aes(x = MET, y = EX))+
  geom_smooth(aes(x = MET, y = EX))+
  theme_bw()

##2

library(tree)
set.seed(12345)

#Fitting tree model on EX~MET, with minimum size by branch of 8 
fit<-tree(EX~MET, data = ordered_data, minsize = 8)
set.seed(12345)
##Doing cross-validation for the best tree
Diff_tree<-cv.tree(fit) ###best size= 3
index_min_dev<-which.min(Diff_tree$dev) ##index 5 (selecting lower sd error of the different models)
best_size<-Diff_tree$size[index_min_dev]## Selecting size = 3 by passing the index of the small error

##Calculating best model
best_model<-prune.tree(fit, best= best_size)

#Best model plot
plot(best_model)
text(best_model, cex=.7)

##EX predicted values on my ordered data
mypred <- predict(best_model, newdata= ordered_data, type = "vector")

#Calculating the residuals from real minus predicted

residuals <- ordered_data$EX-mypred
Pearson<- sum(((ordered_data$EX-mypred)**2)/mypred)

newframe<- data.frame(MET = ordered_data$MET, TrueEX = ordered_data$EX, predictEX= mypred)
ggplot2::ggplot(newframe)+
            geom_point(aes(x = MET, y = TrueEX, color = "True EX"))+
            geom_point(aes(x = MET, y = predictEX, color = "Predict EX"))+
            ylab("EX")+
            theme_bw()

#Histograma of residuals
hist(residuals, col = "orange", border ="black", labels = TRUE)   

# Parece que la distribucion es exponencial pero no tenemos suficientes datos para averiguarlo 

##3



library(boot)

#computing bootstrap samples
f=function(data, ind){
  set.seed(12345)
  data1=data[ind,]# extract bootstrap sample
  res=tree(EX~MET, data=data1,minsize = 8) #fit linear model
  best_tree<-prune.tree(res, best = 3)#predict values for all Area values from the original data
  EXP=predict(best_tree,newdata=ordered_data) 
  return(EXP)
}
result <- boot(ordered_data, f, R=1000)
CI <- envelope(result) #compute confidence bands

tree<-predict(best_model)

plot(ordered_data$MET, ordered_data$EX, pch=21, bg="orange",ylim = c(150,470), ylab = "EX" , xlab = "MET")
points(ordered_data$MET,tree,type="l") #plot fitted line

#plot cofidence bands
points(ordered_data$MET,CI$point[2,], type="l", col="blue")
points(ordered_data$MET,CI$point[1,], type="l", col="blue")


#########4

tree <- best_model

rng<-function(data, tree) {
  data1=data.frame(EX=data$EX, MET=data$MET)
  n=length(data1$EX)
  #generate new EX
  data1$EX=rnorm(n,predict(tree, newdata=data1),sd(residuals(tree)))
  return(data1)
}
f2<-function(data1){
  res <-tree(EX~MET, data=data1, minsize = 8) #fit linear model
  best_tree<- prune.tree(res, best = 3)
  #predict values for all Area values from the original data
  EX<-predict(best_tree,newdata=ordered_data) 
  return(EX)
}
set.seed(12345)
res<-boot(ordered_data, statistic=f2, R=1000, mle=tree,ran.gen=rng, sim="parametric")

CI_param <- envelope(res) #compute confidence bands


best_model_prediction<- predict(best_model, newdata = ordered_data)

##prediction bands
f3=function(data1){
  res=tree(EX~MET, data=data1) #fit linear model
  #predict values for all Area values from the original data
  best_tree<-prune.tree(res, best = 3)
  EX=predict(best_tree,newdata=ordered_data) 
  n=length(ordered_data$EX)
  predictedP=rnorm(n,EX, sd(residuals(best_tree)))
  return(predictedP)
}
result_bands<-boot(ordered_data, statistic=f3, R=1000, mle=best_model,ran.gen=rng, sim="parametric")

Prediction_bands <- envelope(result_bands) #compute confidence bands

plot(ordered_data$MET, ordered_data$EX, pch=21, bg="orange", ylim = c(150,450))
points(ordered_data$MET,best_model_prediction,type="l") #plot fitted line

#plot cofidence bands
points(ordered_data$MET,CI_param$point[2,], type="l", col="blue")
points(ordered_data$MET,CI_param$point[1,], type="l", col="blue")
points(ordered_data$MET,Prediction_bands$point[2,], type="l", col="red")
points(ordered_data$MET,Prediction_bands$point[1,], type="l", col="red")
###Assignment 2


##Reading data
data<- read.csv2("C:/Users/M/Desktop/Statistics and Data Mining Master/Semester 1/Second part of the semester/intro to machine learning/lab4/NIRspectra.csv")
dim(data)
names(data)

##1

#Computing PCA for the data for all components but viscosity(last component)
res <- prcomp(data[,-ncol(data)])
summary(res) # it is seen that just the first two components stand for more than 99%

#alternatively one can check eigenvalues, which are the square roots of the eigen values
lambda <- res$sdev^2 
#proportion of variation of eigenvalues over hundred
proportion_variation<- as.numeric(sprintf("%2.3f",lambda/sum(lambda)*100))
sum(proportion_variation[1:2])##99% of the variance with the first 2 variables
##Screenplot to visualize it
screeplot(res,main = "")

library(ggplot2)
# The scores are meant to be the output x in the prcomp result
scores1 <- res$x[,1]
scores2 <- res$x[,2]

##ggplot of the scores of PC1, PC2 coordinates
ggplot(data= data.frame(scores1= scores1, scores2= scores2)) + 
  geom_point((aes(x = scores1, y =scores2))) +
  xlab("PC1")+ ylab("PC2")

##2
###the loadings are the rotation prcomp
U<- as.data.frame(res$rotation)
dim(U)

##Trace plot of the eigenvectors to each variable
ggplot2::ggplot(U)+
  geom_line(aes(x= 1:nrow(U), y = PC1, color = "PC1"))+
  geom_line(aes(x= 1:nrow(U), y = PC2, color = "PC2"))+ 
  coord_cartesian(ylim = c(-0.07, 0.12)) + 
  ylab("PC1 and PC2 eigenvector values")+
  xlab("index")+
  theme_bw()

##3 

set.seed(12345)
#install.packages("fastICA")
library(fastICA)

#ICA formula using the first 2 components
myICA <- fastICA(data[,-ncol(data)], 2, alg.typ = "parallel", fun = "logcosh", alpha = 1, 
                 method = "R", row.norm = FALSE, maxit = 200, tol = 0.0001, verbose = TRUE) 

W_prime<-myICA$K%*%myICA$W ##multiplication of W*K

#trace scores1 vs scores2
ggplot2::ggplot(data.frame(PC1=myICA$S[,1], PC2=myICA$S[,2]))+
  geom_point(aes(x= PC1, y = PC2))+
  ylab("PC2")+
  xlab("PC1")+
  theme_bw()



ggplot2::ggplot(data.frame(PC1=W_prime[,1], PC2=W_prime[,2]))+
  geom_line(aes(x= 1:nrow(W_prime), y = PC1, color = "PC1"))+
  geom_line(aes(x= 1:nrow(W_prime), y = PC2, color = "PC2"))+ 
  #coord_cartesian(ylim = c(0, 0.12)) + 
  ylab("PC1 and PC2 scores")+
  xlab("index")+
  theme_bw()


##4
set.seed(12345)
#install.packages("pls")
library(pls)
pcr.fit <- pcr(Viscosity~., data=data, validation="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP")



