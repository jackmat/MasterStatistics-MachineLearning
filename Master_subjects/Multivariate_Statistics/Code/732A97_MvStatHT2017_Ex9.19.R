X<-read.table("C:/Users/Carles/Desktop/MasterStatistics-MachineLearning/Master_subjects/Multivariate_Statistics/Code/T9-12.DAT",col.names=c("Growth","Profits","Newaccts","Creative","Mechanic","Abstract","Math"))
p<-ncol(X)
n<-nrow(X)
Z<-X

## a)
mu<-apply(X,2,mean)
s<-apply(X,2,sd)
for (i in 1:p){Z[,i]<-(X[,i]-mu[i])/s[i]}

F2res<-factanal(Z,2,rotation="none") ## ML solution
L2<-matrix(F2res$loadings,ncol=2)
F3res<-factanal(Z,3,rotation="none") ## ML solution
L3<-matrix(F3res$loadings,ncol=3)
rownames(L2)<-colnames(X)
rownames(L3)<-colnames(X)


## b)
F2Vres<-factanal(Z,2,rotation="varimax") ## ML solution
LV2<-matrix(F2Vres$loadings,ncol=2)
F3Vres<-factanal(Z,3,rotation="varimax") ## ML solution
LV3<-matrix(F3Vres$loadings,ncol=3)

rownames(LV2)<-colnames(X)
rownames(LV3)<-colnames(X)

diag(L2%*%t(L2))==diag(LV2%*%t(LV2))

diag(L3%*%t(L3))==diag(LV3%*%t(LV3))

## c)
R<-cor(Z)
h2<-1-diag(L2%*%t(L2))
Resid2<-R-L2%*%t(L2)-diag(h2)
hV2<-1-diag(LV2%*%t(LV2))
ResidV2<-R-LV2%*%t(LV2)-diag(hV2)
h3<-1-diag(L3%*%t(L3))
Resid3<-R-L3%*%t(L3)-diag(h3)
hV3<-1-diag(LV3%*%t(LV3))
ResidV3<-R-LV3%*%t(LV3)-diag(hV3)


# d) 
Sn<-((n-1)/n)*cov(Z)
alpha<-0.01

m<-2
CritValue2<-qchisq(1-alpha,((p-m)^2-p-m)/2)
TestStat2<-(n-1-(2*p+4*m+5)/6)*log(det(L2%*%t(L2)+diag(h2))/det(Sn))

m<-3
CritValue3<-qchisq(1-alpha,((p-m)^2-p-m)/2)
TestStat3<-(n-1-(2*p+4*m+5)/6)*log(det(L3%*%t(L3)+diag(h3))/det(Sn))

# e)
x<- c(110,98,105,15,18,12,35)
z<- (x-mu)/s ## standardize
f<- t(L3)%*%solve(R)%*%z ## regression method, Eq. 9-58
