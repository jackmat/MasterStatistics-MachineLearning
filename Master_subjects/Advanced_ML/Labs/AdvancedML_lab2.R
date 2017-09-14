
###################Lab1

set.seed(12345)
#install.packages("HMM")
library(HMM)

##QUestion1
# Initialise HMM
states          <- as.character(1:10)
symbols         <- as.character(1:10) 
startProbs      <- rep(0.1,10) 
transProbs      <- diag(x = 0.5, 10)
transProbs[10,1]<- 0.5 #writing the ones left
emissionProbs   <- diag(x = 0.2, 10)
emissionProbs[1,9:10]<-rep(0.2,2)
emissionProbs[2,10]<-rep(0.2,1)
emissionProbs[9,1]<-rep(0.2,1)
emissionProbs[10,1:2]<-rep(0.2,1)
for(i in 1:ncol(transProbs)){
  for(j in 1:nrow(transProbs)){
    if(j == i+1){
      transProbs[i,j]<-0.5
      emissionProbs[i,j]<-0.2}
    if(j == i+2){ emissionProbs[i,j]<-0.2}
    if(j == i-1){ emissionProbs[i,j]<-0.2}
    if(j == i-2){ emissionProbs[i,j]<-0.2}
}}
  

myhmm = initHMM(States = states, 
              Symbols = symbols, 
              startProbs = startProbs,
              transProbs= transProbs,
              emissionProbs=emissionProbs)

##2
length<-100

my100sim<-simHMM(myhmm, length)


#3

myobs<-my100sim$observation # Taking just the real observations (Z)
###Filtering
filtering<-exp(forward(myhmm, observation=myobs))#A matrix containing the forward probabilities given on a logarithmic scale (natural logarithm).
marginalFilter<-apply(as.data.frame(filtering),2,  FUN = function(x){prop.table(x)})##prop.table already calculated the % on 1.


###Smoothing
smoothing<-posterior(myhmm, observation=myobs)

##Most probable path

Viterbi<-viterbi(myhmm, observation=myobs)

#4
mystates<-my100sim$states


smoothingMostProb   <- sapply(as.data.frame(smoothing), which.max)
FilteringMostProb   <- sapply(as.data.frame(marginalFilter),which.max)
Viterbi

smoothingresult <-table(mystates == smoothingMostProb)
Filteringresult <-table(mystates == FilteringMostProb)
Viterbiresult   <-table(mystates == Viterbi)

ResultTable     <-cbind(smoothingresult, Filteringresult, Viterbiresult)

###5


Comparingsimuations<-function(HMM, length){
  ResultTable<-list()
  FilterEntropy<-list()
  library(entropy)
  for(i in 1:length(length)){
    my100sim              <-simHMM(myhmm, length[i])
    myobs                 <-my100sim$observation # Taking just the real observations (Z)
    ###Filtering
    filtering             <-exp(forward(myhmm, observation=myobs))#A matrix containing the forward probabilities given on a logarithmic scale (natural logarithm).
    marginalFilter        <-apply(as.data.frame(filtering),2,  FUN = function(x){prop.table(x)})##prop.table already calculated the % on 1.
    ###Smoothing
    smoothing             <-posterior(myhmm, observation=myobs)
    
    
    #Getting just the Z states for different models
    mystates              <-my100sim$states
    smoothingMostProb     <- sapply(as.data.frame(smoothing), which.max)
    FilteringMostProb     <- sapply(as.data.frame(marginalFilter),which.max)
    
    smoothingresult       <-table(mystates == smoothingMostProb)
    Filteringresult       <-table(mystates == FilteringMostProb)
    Viterbi               <-viterbi(myhmm, observation=myobs)##Most probable path
    
    Viterbiresult         <-table(mystates == Viterbi)
    ResultTable[[i]]      <-cbind(smoothingresult, Filteringresult, Viterbiresult)/length[i]
  
    
    FilterEntropy[[i]]    <- apply(marginalFilter,2,entropy.empirical)

  }
 return(list(PercentageSIm=ResultTable, Entropy=FilterEntropy)) 
}  


length  <-rep(100,5)
myhmm = initHMM(States = states, 
                Symbols = symbols, 
                startProbs = startProbs,
                transProbs= transProbs,
                emissionProbs=emissionProbs)

Simulations<-Comparingsimuations(myhmm, length = length)

Simulations$PercentageSIm

#6
#install.packages("entropy")

Entropy<-Simulations$Entropy

plot(Entropy[[1]], col = "red")
lines(Entropy[[2]], col = "blue")
lines(Entropy[[3]], col = "green")
lines(Entropy[[4]], col = "purple")
lines(Entropy[[5]], col = "black")

##7

