library(calibrate) ## for textxy()

## The original data file T12-7.DAT has to be made R friendly first ...
mD<-read.table("T12-7_Rfriendy.DAT",sep=";",header=TRUE)
mD[upper.tri(mD,diag=FALSE)]<-mD[lower.tri(mD,diag=FALSE)]

fit2<-cmdscale(mD,k=2)
plot(fit2,pch=19,xlab="",ylab="",main="",xlim=c(-600+min(fit2[,1]),600+max(fit2[,1])),ylim=c(-600+min(fit2[,2]),600+max(fit2[,2])))
textxy(fit2[,1],fit2[,2],colnames(mD),cex=1)


fit3<-cmdscale(mD,k=3)
fit5<-cmdscale(mD,k=5)


mDnorm<-mD/max(mD)
mDfit2<-as.matrix(dist(fit2));mDfit2norm<-mDfit2/max(mDfit2);Penalty_q2<-sqrt(sum((mDnorm-mDfit2norm)^2))
mDfit3<-as.matrix(dist(fit3));mDfit3norm<-mDfit3/max(mDfit3);Penalty_q3<-sqrt(sum((mDnorm-mDfit3norm)^2))
mDfit5<-as.matrix(dist(fit5));mDfit5norm<-mDfit5/max(mDfit5);Penalty_q5<-sqrt(sum((mDnorm-mDfit5norm)^2))
## here we improve with q

Penalty_q2_rel<-sqrt(sum(((mDnorm-mDfit2norm)^2)/mDnorm^2,na.rm=TRUE)) ## this one actually seems best
Penalty_q3_rel<-sqrt(sum(((mDnorm-mDfit3norm)^2)/mDnorm^2,na.rm=TRUE))
Penalty_q5_rel<-sqrt(sum(((mDnorm-mDfit5norm)^2)/mDnorm^2,na.rm=TRUE))

fit2eig<-cmdscale(mD,k=2,eig=TRUE) ## return eigenvalues (as described in the next part of the lecture)
Criterion1<-cumsum(fit2eig$eig^2)/sum(fit2eig$eig^2) ## some eigenvalues here are negative, as B />=0
## q=2,3 seems OK

Criterion2<-cumsum(abs(fit2eig$eig))/sum(abs(fit2eig$eig)) ## some eigenvalues here are negative, as B />=0
## q=2,3 not as good
