

Loadings<- matrix(c(0.602,0.467,0.926,1,0.874,0.894,0.2,0.154,0.143,0,0.476,0.327),ncol = 2)
communalities<-diag(Loadings%*%t(Loadings))##communalities
specificVar<-1-communalities #specific variance


varimaxrotated<- matrix(c(0.484,0.375,0.603,0.519,0.861,0.744,0.411,0.319,0.717,0.855,0.499,0.594), ncol =2)

diag(varimaxrotated%*%t(varimaxrotated))

sum(Loadings[,1]**2)/6
#should be properly written
cormatrix= matrix(c(1,0.505,0.569,0.602,0.621,0.603,0.505,1,0.422,0.467,0.482,0.450,0.569,0.422,1,0.926,0.877,0.878,0.602,0.467,0.926,1,0.874,0.937,0.603,0.45,0.878,0.894,0.937,1),6,6)

cormatrix-communalities-diag(specificVar)

Loadings*
varimax(Loadings, normalize = F)
varimax(varimaxrotated)
p<- 6
res0<-t(apply(Loadings, 1, FUN= function(x){x/sqrt(sum(x**2))}))
res1<-t(apply(varimaxrotated, 1, FUN= function(x){x/sqrt(sum(x**2))}))

1/p*sum(colSums(res0**4)- (colSums(res0^2)^2)/p)

1/p*sum(apply(res1,2,FUN = function(x){sum(x**4)})-
          apply(res1,2,FUN = function(x){sum(x**2)**2})/p)
