##Exam solutions


#1
library(bnlearn)
data(asia)


####3
pdag = iamb(asia)
pdag
colnames(asia)
plot(pdag)
dag = set.arc(pdag, from = "B", to = "A")
dag = pdag2dag(pdag, ordering = colnames(asia))
plot(dag)
fit = bn.fit(dag, asia)##It creates all conditional tables from one edge with another with his possible outcomes
LS<- as.grain(fit)## it creates LS

MM<- compile(LS)##it triangulates and moralizes
plot(MM)
basic<-querygrain(MM, nodes = c("A"))


##Exact one
ChangeEvidence5<-setEvidence(MM, c("X","B"), c("yes", "yes"))
finalstate5<-querygrain(ChangeEvidence5, nodes = c("A"))
##Approximate algorithm
rsample5<-cpquery(fit, event = c(A== "yes"), 
                  evidence= (X=="yes")&(B=="yes"), method = "ls")


###

library(bnlearn)
burn_in<- c(10000)
every<-c(100)

nodes<- LETTERS[1:5]
set.seed(12345)
num <- 10000
checkingDags<- function(burnin, every, num, nodes){
  result<- integer(num)
  mymat<- matrix(ncol =length(burnin), nrow=length(every))
  for(i in 1:length(burnin)){
    for(j in 1:length(every)){
      z<-random.graph(nodes = nodes, num = num,  method = "ic-dag", 
                      every = every[j], burn.in =burnin[i])
      
      m<-unique(z)
      print(length(m))# This gives the proportion the number of repeated graphs out my 
      
      moral<-lapply(m, FUN= function(x){moral(x)})
      skeleton<-lapply(m, FUN= function(x){skeleton(x)})
      for(i in 1: length(skeleton)){
        result[i]<-all.equal(skeleton[[i]], moral[[i]])
      }
      
    }
  }
  return(result)
}

result<-checkingDags(burnin = burn_in, every = every, num = num, nodes = nodes)
colnames(result)<- paste0("burn in = " , burn_in)
rownames(result)<- paste0("every = " , every)
result

#############2
# Initialise HMM
library(HMM)
states          <- as.character(1:10)
symbols         <- as.character(1:11) 
startProbs      <- c(rep(0.1,10),0) 

transProbs      <- diag(x = 0.5, 10)
transProbs[10,1]<- 0.5 #writing the ones left
emissionProbs   <- diag(x = 0.1, 10)
emissionProbs[1,9:10]<-rep(0.1,2)
emissionProbs[2,10]<-rep(0.1,1)
emissionProbs[9,1]<-rep(0.1,1)
emissionProbs[10,1:2]<-rep(0.1,1)
for(i in 1:ncol(transProbs)){
  for(j in 1:nrow(transProbs)){
    if(j == i+1){
      transProbs[i,j]<-0.5
      emissionProbs[i,j]<-0.1}
    if(j == i+2){ emissionProbs[i,j]<-0.1}
    if(j == i-1){ emissionProbs[i,j]<-0.1}
    if(j == i-2){ emissionProbs[i,j]<-0.1}
  }}

Emission<-cbind(emissionProbs, rep(0.5,10))
TransProbs
myhmm = initHMM(States = states, 
                Symbols = symbols, 
                startProbs = startProbs[1:10],
                transProbs= transProbs[1:10,1:10],
                emissionProbs=Emission)

##2
length<-5
myobs<-  c("1","11","11","11")
###Filtering
filtering<-exp(forward(myhmm, observation=myobs))#A matrix containing the forward probabilities given on a logarithmic scale (natural logarithm).
marginalFilter<-apply(as.data.frame(filtering),2,  FUN = function(x){prop.table(x)})##prop.table already calculated the % on 1.


###Smoothing
smoothing<-posterior(myhmm, observation=myobs)
apply(smoothing, 2, which.max)
##Most probable path

Viterbi<-viterbi(myhmm, observation=myobs)

#4


smoothingMostProb   <- sapply(as.data.frame(smoothing), which.max)
FilteringMostProb   <- sapply(as.data.frame(marginalFilter),which.max)
Viterbi

smoothingresult <-table(mystates == smoothingMostProb)
Filteringresult <-table(mystates == FilteringMostProb)
Viterbiresult   <-table(mystates == Viterbi)

ResultTable     <-cbind(smoothingresult, Filteringresult, Viterbiresult)


###2.2
transProbs
colnames(transProbs)<-c("1","1","2","2","3","3","4","4","5","5")
rownames(transProbs)<-c("1","1","2","2","3","3","4","4","5","5")
StartProbs<-startProbs[1:5]
Emi<-diag(1/3,10)
Emi[1,]<-c(1/3,0,1/3,0,0,0,0,0,1/3,0)
Emi[2,]<-c(0,1/3,0,0,0,0,0,1/3,0,1/3)
Emi[3,]<-c(1/3,0,0,0,0,0,1/3,0,1/3,0)
Emi[4,]<-c(0,0,0,0,0,1/3,0,1/3,0,1/3)
Emi[5,]<-c(0,0,0,0,1/3,0,1/3,0,1/3,0)
Emi[6,]<-c(0,0,0,1/3,0,1/3,0,1/3,0,0)
Emi[7,]<-c(0,0,1/3,0,1/3,0,1/3,0,0,0)
Emi[8,]<-c(0,1/3,0,1/3,0,1/3,0,0,0,0)
Emi[9,]<-c(1/3,0,1/3,0,1/3,0,0,0,0,0)
Emi[10,]<-c(0,1/3,0,1/3,0,0,0,0,0,1/3)


myhmm2_2 = initHMM(States = c("1a","1b","2a","2b","3a","3b","4a","4b","5a","5b"), 
                Symbols = c("1a","1b","2a","2b","3a","3b","4a","4b","5a","5b"), 
                startProbs = rep(0.1,10),
                transProbs= transProbs,
                emissionProbs=Emi)


simHMM(myhmm2_2,100)

b<-initHMM(c("X","Y"), c("a","b","c"))
simHMM(b,10)



##3


# Squared exponential kernel
# Squared exponential kernel
k1 <- function(sigmaf = 1, ell = 1)  
{   
  rval <- function(x, y = NULL) 
  {	if(class(y) == "NULL"){
    r<-x
  }else {       
    r = sqrt(crossprod(x-y))       
  }
    return(sigmaf^2*exp(-r^2/(2*ell^2)))     
  }   
  class(rval) <- "kernel"   
  return(rval) 
}


k2 <- function(sigmaf = 1, ell = 1, alpha = 1)  
{   
  rval <- function(x, y = NULL) 
  {	if(class(y) == "NULL"){
    r<-x
    }else{     
    r = sqrt(crossprod(x-y))
    }
  return(sigmaf^2*(1+r^2/(2*alpha*ell^2))^-alpha)   
  }   
  class(rval) <- "kernel"   
  return(rval) 
} 


k3 <- function(sigmaf = 1, ell = 1)  
{   
  rval <- function(x, y = NULL) 
  {	if(class(y) == "NULL"){
    r<-x
  }else{
    r = sqrt(crossprod(x-y))
  }
  return(sigmaf^2*(1+sqrt(3)*r/ell)*exp(-sqrt(3)*r/ell))   
  }   
  class(rval) <- "kernel"   
  return(rval) 
} 


sigmaf<-1
ell<-1

alpha<-c(1/2,2,20)
ar<-seq(0,4, by= 0.1)
k1top<-k1()
first<-k1top(x=ar)
result<-list()
k2(sigmaf)
for(i in 1:length(alpha)){
  z<- k2(sigmaf = sigmaf, ell = ell, alpha = alpha[i])
  result[[i]]<- z(x = ar)
  
  
}
result
