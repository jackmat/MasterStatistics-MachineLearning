##Advanced Machine Learning lab 1


# source("http://bioconductor.org/biocLite.R")
# biocLite(c("graph", "RBGL", "Rgraphviz"))
#install.packages("gRain", dependencies=TRUE)
#install.packages("bnlearn")
#install.packages("gRbase")
#install.packages("gRain")
library(bnlearn)
library(gRain)
library(gRbase)

###1
data(asia)

plot(asia)

par(mfrow=c(1,2))

wl = matrix(c("E", "T"), ncol = 2, byrow = TRUE,
            dimnames = list(NULL, c("from", "to")))

plot(hc(asia, whitelist = wl), main ="HC with initial constraint")
plot(hc(asia, whitelist = NULL),main ="HC without constraint")

mygraph<-cpdag(hc(asia, whitelist = NULL, restart = 10))
cmygraph<-cpdag(hc(asia, whitelist = wl))
mygraph
cmygraph
all.equal(mygraph, cmygraph)

#################Alternative
set.seed(12345)
countinue <- TRUE
while(countinue){
  hc1 <- hc(alarm, restart = 10)
  hc2 <- hc(alarm, restart = 10)
  continue <- ifelse(all.equal(vstructs(hc1), vstructs(hc2)) == TRUE, TRUE, FALSE)
  if (continue !=TRUE){
    par(mfrow = c(1,2))
    graphviz.compare(hc1, hc2)
    par(mfrow = c(1,1))
    break
  }
}

###2
data("alarm")
n<-4
iss<- c(1,25,50,100)
mydag<-list()
par(mfrow=c(2,2))
for(i in 1:n){
  mydag[[i]]<- hc(alarm, restart = n,  
                  score = "bde", iss = iss[i])
  plot(mydag[[i]], sub = paste0("iss = ", iss[i] ))
  print(alpha.star(mydag[[i]], alarm, debug = FALSE))
}


par(mfrow=c(1,1))

####3
data(learning.test)
pdag = iamb(learning.test)
pdag
plot(pdag)
dag = set.arc(pdag, from = "B", to = "A")
dag = pdag2dag(pdag, ordering = c("A", "B", "C", "D", "E", "F"))
plot(dag)
LS<- as.grain(fit)## it creates LS
plot(LS)
fit = bn.fit(dag, learning.test)##It creates all conditional tables from one edge with another with his possible outcomes

MM<- compile(LS)##it triangulates and moralizes
plot(MM)
basic<-querygrain(MM, nodes = c("A","E"))

###################
###################comparison of results for one change
## I want the conditional distribution of A, E given B = b

##Exact one
ChangeEvidence<-setEvidence(MM, c("B"), c("b"))
finalstate<-querygrain(ChangeEvidence, nodes = c("A","E"))

##Greedy algorithm
rsample<-cpdist(fit, nodes = c("A", "E"), evidence= (B=="b"), method = "ls")
condprob<-lapply(rsample, FUN = function(x){table(x)/length(x)})

res1<-cbind(A_LS= as.data.frame(finalstate)[1], A_Greedy= condprob$A,
      E_LS= as.data.frame(finalstate)[2], E_Greedy= condprob$E)
res1<-res1[,c(1,3,4,6)]
colnames(res1)<- c("A_LS", "A_Greedy","E_LS", "E_Greedy" )
res1

###################comparison of results for two changes
## I want the conditional distribution of A, E given B = b and F = b
ChangeEvidence2<-setEvidence(MM, c("B","F"), c("b", "b"))
finalstate2<-querygrain(ChangeEvidence2, nodes = c("A","E"))


res2<- list()
for(i in 1:4){
  ##Exact one
  ChangeEvidence2<-setEvidence(MM, c("B","F"), c("b", "b"))
  finalstate2<-querygrain(ChangeEvidence2, nodes = c("A","E"))
  ##Greedy algorithm
  rsample2<-cpdist(fit, nodes = c("A", "E"), evidence= (B=="b")&(F=="b"), method = "ls")
  condprob2<-lapply(rsample2, FUN = function(x){table(x)/length(x)})
  
  res2[[i]]<-cbind(A_LS= as.data.frame(finalstate2)[1], A_Approx= condprob2$A,
              E_LS= as.data.frame(finalstate2)[2], E_Approx= condprob2$E)
  res2[[i]]<-res2[[i]][,c(1,3,4,6)]
  
}
res2


###wITHOUT EVIDENCE
## I want the conditional distribution of A, E 

##Exact one
finalstate3<-querygrain(MM, nodes = c("A","E"))


###################comparison of results for two changes
## I want the conditional distribution of A, E given B = b and F = b, D=d
ChangeEvidence4<-setEvidence(MM, c("B","F","D", "A"), c("b", "b","b", "b"))
finalstate4<-querygrain(ChangeEvidence4, nodes = c("A","E"))

i<-1
res5<- list()
for(i in 1:4){
  ##Exact one
  ChangeEvidence5<-setEvidence(MM, c("B","F","D", "C"), c("b", "b","b", "b"))
  finalstate5<-querygrain(ChangeEvidence5, nodes = c("A","E"))
  ##Greedy algorithm
  rsample5<-cpdist(fit, nodes = c("A", "E"), evidence= (B=="b")&(F=="b")&(D=="b")&(C=="b"), method = "ls")
  condprob5<-lapply(rsample5, FUN = function(x){table(x)/length(x)})
  
  res5[[i]]<-cbind(A_LS= data.frame(finalstate5)[1], A_Approx= data.frame(condprob5$A),
                   E_LS= data.frame(finalstate5)[2], E_Approx= data.frame(condprob5$E))
  res5[[i]]<-res5[[i]][,c(1,3,4,6)]
  
}
res5



###4
burn_in<- c(1,100, 10000, 1000000)
every<-c(2,20,100, 200)

nodes<- LETTERS[1:5]
num <- 1000


checkingDags<- function(burnin, every, num, nodes){
  mymat<- matrix(ncol =length(burnin), nrow=length(every))
  for(i in 1:length(burnin)){
    for(j in 1:length(every)){
      z<-random.graph(nodes = nodes, num = num,  method = "ic-dag", 
                      every = every[j], burn.in =burnin[i])
      
      m<-unique(z)
      print(length(m))# This gives the proportion the number of repeated graphs out my 
      
      dagGraph<-lapply(m, FUN= function(x){cpdag(x)})
      mymat[j,i]<- length(unique(dagGraph))/length(m)
      }
  }
  return(mymat)
}

result<-checkingDags(burnin = burn_in, every = every, num = num, nodes = nodes)
colnames(result)<- paste0("burn in = " , burn_in)
rownames(result)<- paste0("every = " , every)
result
