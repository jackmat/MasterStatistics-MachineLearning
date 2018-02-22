8**(-1/2)
1/(2*sqrt(2))*2*1/5*2*1/sqrt(8)

covMat<- matrix(c(8,2,3,1,2,5,-1,3,3,-1,6,-2,1,3,-2,7),4,4)
eigen(cor(covMat))
cov11<- cor(covMat[1:2,1:2])
cov12<-cor(covMat[1:2,3:4])
cov21<- cor(covMat[3:4,1:2])
cov22<-cor(covMat[3:4,3:4])
inv11<-sqrt(solve(cov11))
inv22<-solve(cov22)
inv11%*%cov12%*%inv22%*%cov21%*%inv11
cancor(cov11, cov12)

cov14<-covMat[1:2,1:4]
cov24<- covMat[3:4, 1:4]
cancor(cov14, cov24)

sqrt(1/8)*3*1/6*3*sqrt(1/8)
library(boot)

headsize.std <- sweep(frets, 2,  apply(frets, 2, sd), FUN = "/") 

R <- cor(headsize.std) 
r11 <- R[1:2, 1:2] 
r22 <- R[3:4, 3:4] 
r12 <- R[1:2, 3:4] 
r21 <- R[3:4, 1:2] 
E1 <- solve(r11) %*% r12 %*% solve(r22) %*%r21
E2 <- solve(r22) %*% r21 %*% solve(r11) %*%r12
e1 <- eigen(E1)
e2 <- eigen(E2)

girth1 <- headsize.std[,1:2] %*% e1$vectors[,1]
girth2 <- headsize.std[,3:4] %*% e2$vectors[,1]
