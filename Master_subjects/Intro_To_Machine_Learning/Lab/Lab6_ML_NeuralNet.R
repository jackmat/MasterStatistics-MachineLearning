##Assignment 1

#install.packages("neuralnet")
library(neuralnet)
set.seed(1234567890)


Var <- runif(50, 0, 10)
trva <- data.frame(Var, Sin=sin(Var))
tr <- trva[1:25,] # Training
va <- trva[26:50,] # Validation

# Random initializaiton of the weights in the interval [-1, 1]
d<- 10 #number of units from the hidden layer

n_tr<- dim(tr)[1]
n_va<- dim(va)[1]

start_weights<- runif(50, min = -1, max = 1) 


MSE <- integer(0) ## vector of minimum MSE
  for(i in 1:10) {
    nn <- neuralnet(
      Sin~Var,
      data = tr, hidden=10, threshold = i/1000, startweights = start_weights )
    
    pr.nn <- compute(x = nn,covariate = va$Var) 
    
    
    MSE[i]<- sum((va$Sin- pr.nn$net.result)^2)/nrow(va)      # Your code here
  }
library(ggplot2)
ggplot2::ggplot(as.data.frame(winit))+
          geom_line(aes(x =seq(1/1000,10/1000,1/1000), y = winit))+
          geom_point(aes(x =seq(1/1000,10/1000,1/1000), y = winit))+
          xlab("threshold sequence")+ ylab("MSE")+ theme_bw()

index_best_MSE<-which.min(winit)## Index of minimum position
my_seq <- seq(1/1000,10/1000,1/1000)

plot(nn <- neuralnet(Sin~Var,data = tr, hidden=10,
  threshold = my_seq[index_best_MSE]))
  # Plot of the predictions (black dots) and the data (red dots)
  plot(prediction(nn)$rep1)
  points(trva, col = "red")
  