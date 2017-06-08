### Block 2 Lab 2b ### 

## EM algorithm ##

set.seed(1234567890)

max_it <- 100 # max number of EM iterations
min_change <- 0.1 
# min change in log likelihood between two consecutive EM iterations
N=1000 # number of training points
D=10 # number of dimensions
x <- matrix(nrow=N, ncol=D) # training data
true_pi <- vector(length = 3) # true mixing coefficients
true_mu <- matrix(nrow=3, ncol=D) # true conditional distributions
true_pi=c(1/3, 1/3, 1/3)
true_mu[1,]=c(0.5,0.6,0.4,0.7,0.3,0.8,0.2,0.9,0.1,1)
true_mu[2,]=c(0.5,0.4,0.6,0.3,0.7,0.2,0.8,0.1,0.9,0)
true_mu[3,]=c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5)
plot(true_mu[1,], type="o", col="blue", ylim=c(0,1), ylab="true mu",
     main="True mu")
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

for(it in 1:max_it) {
  plot(mu[1,], type="o", col="blue", ylim=c(0,1), ylab="Estimates", 
       main="EM algorithm estimates")
  points(mu[2,], type="o", col="red")
  points(mu[3,], type="o", col="green")
  #points(mu[4,], type="o", col="yellow")
  Sys.sleep(0.5)
  # E-step: Computation of the fractional component assignments
  
  #post prob
  probx_mu<- 0
  
  for (n in 1:N) {
    prob<- 0
    for (k in 1:K) {
      prob <- prob + (pi[k] * prod((mu[k,]^x[n,]) * (1-mu[k,])^(1-x[n,])))
    }
    probx_mu[n]<- prob
  }
  
  # z-matrisen
  for (n in 1:N) {
    for (k in 1:K) {
      z[n,k] <-(pi[k]*(prod((mu[k,]^x[n,])*(1-mu[k,])^(1-x[n,]))))/probx_mu[n]
    }
  }
  
  #Log likelihood computation.
  newLike<-matrix(0,nrow=1000,ncol=3)
  for (n in 1:N) {
    for (k in 1:K) {
      newLike[n,k] <- (pi[k]*(prod((mu[k,]^x[n,]) * (1-mu[k,])^(1-x[n,]))))
    }
  }
  sum_newLike<-rowSums(newLike)
  llik[it]<-sum(log(sum_newLike))
  
  cat("iteration: ", it, "log likelihood: ", llik[it], "\n")
  flush.console()
  
  # Stops the function once convergence reached
  if (it > 2){
    oldlog<-llik[it-1]
    if (abs((abs(oldlog)- abs(llik[it]))) < min_change){
      stop(print("Convergence reached"))
    }
  }
  
  #M-step: ML parameter estimation from the data and 
  #fractional component assignments
  #compute estimates of pi
  for (k in 1:K){
    pi <- colSums(z)/N
  }
  
  #compute estimates of mu
  for (d in 1:D) {
    for (k in 1:K) {
      mu[k,d] <-  sum(x[,d] * z[,k])/sum(z[,k])
    }
  }
}

pi
mu

plot(llik[1:it], type="o", ylab="Log-likelihood", main="Log-likelihood",
     xlab="Number of iteration", col="blue")

