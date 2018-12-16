
#Packages install
install.packages("bnlearn")
install.packages("http://www.bnlearn.com/releases/bnlearn_latest.tar.gz")
source("http://bioconductor.org/biocLite.R")
biocLite(c("graph", "Rgraphviz", "RBGL"))
install.packages("gRain")

#Libraries
library(gRain)
library(bnlearn)


#Creating Bayesian network structures
e <- empty.graph(LETTERS[1:6])
class(e)

#With a specific arc set
arc.set <- matrix(c("A","C","B","F","C", "F"),
                  ncol = 2, byrow = TRUE,
                  dimnames = list(NULL, c("from", "to")))

arcs(e) <- arc.set
e
