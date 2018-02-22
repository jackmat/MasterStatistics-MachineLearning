## 732A97 Multivariate Statistical Methods
## Krzysztof Bartoszek, STIMA, IDA, LiU
## Code for Lecture 1 2017-11-09

## Ex 1.17/43
## data mentioned in book cannot be directly found on the publisher's www
## download alternative data:
## mvadata.zip, MVAexercise R library (problem with installing, you need to create your own NAMESPACE file)
## from https://www.karlin.mff.cuni.cz/~hlavka/stat.html

athleticrecordsHH<-read.table("athleleticrecord.dat",row.names=1)
## column names are for some reason not in the dat file
colnames(athleticrecordsHH)<-c("100m (s)","200m (s)","400m (s)","800m (s)","1500m (min)","5000m (min)","10000m (min)","Marathon (min)")
apply(athleticrecordsHH,2,mean)
cov(athleticrecordsHH)
cor(athleticrecordsHH)
heatmap(cor(athleticrecordsHH),Rowv=NA,Colv=NA)

athleticrecordsJW<-read.table("T1-9.dat",row.names=1)
## column names are for some reason not in the dat file
colnames(athleticrecordsJW)<-c("100m (s)","200m (s)","400m (s)","800m (s)","1500m (min)","3000m (min)","Marathon (min)")
apply(athleticrecordsJW,2,mean)
cov(athleticrecordsJW)
cor(athleticrecordsJW)
heatmap(cor(athleticrecordsJW),Rowv=NA,Colv=NA)
# ==========================================================================

