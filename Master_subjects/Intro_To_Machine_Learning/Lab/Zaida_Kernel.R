set.seed(1234567890)
library(geosphere)
stations <- read.csv("stations.csv")
temps <- read.csv("temps50k.csv")
st <- merge(stations,temps,by="station_number")

a <- 58.4274 # latitude 
b <- 14.826  #longitude 


longlat<-matrix(c(st[,1],st[,5], st[,4]), ncol=3)#matrix of longitude and lat 
dist0<-matrix(0,ncol=2,nrow=50000)

for (i in 1:50000){
  long<-longlat[i,][2] #longitud
  lat<-longlat[i,][3] #latitud 
  dist0[i,][1]<-distHaversine(c(long,lat), c(b,a))/10000 # distans i mil 
  dist0[i,][2]<-longlat[i,][1] #station nummer 
}

varians<-var(dist0[,1]) #varians of distances 
karnels<-numeric()   
for (i in 1:50000){
  avst<-dist0[i,1]
  karnels[i]<-exp(-(avst**2)/varians*2) #gaus kernels
}

predframe<-matrix(c(st[,11],karnels), ncol=2) #df with kernels and temperature 

namnare<-sum(karnels)  #denominator 
sum(predframe[,1]*predframe[,2])/ namnare #predicted 


######kernel for day distance  

st[,"date"]<-as.Date(st[,"date"]) #making to date format 
preddate<-date  # exemplet for date 2013-11-07

disttime<-numeric()
for (i in 1:50000){
  dy<-difftime(preddate, st[i,"date"], units = "days")#dist in days 
  disttime[i]<-dy[[1]] #the number of days 
}
disttime<-abs(disttime)#dist in absolute value 
varianstime<-var(disttime) #varians in distance 
karnelstime<-numeric()
for (i in 1:50000){   #kernel for days 
  avst<-disttime[i]  
  karnelstime[i]<-exp(-(avst**2)/varianstime*2)
}
##yhat 
predframetid<-matrix(c(st[,11],karnelstime), ncol=2) #df with temp and kernels 
namnaretid<-sum(karnelstime) #denominator 
sum(predframetid[,1]*predframetid[,2])/ namnaretid # predicted temp 

######kernel for hour distance 
times <- c("04:00:00", "06:00:00", "08:00:00", "10:00:00", "12:00:00", "14:00:00",
           "16:00:00", "18:00:00", "20:00:00", "22:00:00", "24:00:00")

times<- as.character(times)
##hours from midnight 
obstime<-as.difftime(as.character(st$time), format="%H:%M:%S",units="hours")
wishtime<-as.difftime(as.character(times), format="%H:%M:%S", units="hours")


disthours<- matrix(0,ncol=11,nrow=50000)
#differences 
for (i in 1:50000){
  for (j in 1:11){
    dif<- (obstime[i] - wishtime[j])
    disthours[i,j]<- dif
  }
}
head(disthours)
#absoult distance 
disthours<-abs(disthours)
head(disthours)

summary(disthours)
##kernels 
karnelshours<- matrix(0,nrow=50000, ncol=11)
for (j in 1:11){
  skillnad<- c()
  varhours<-var(disthours[,j])
  for(i in 1:50000){
    skillnad[i]<- disthours[i,j]
    karnelshours[i,j]<-exp(-(skillnad[i]**2)/varhours*2) #gaus kernels
  }
}
##bindging kernels and temps 
predframehour<-cbind(karnelshours, st[,11])

#pred
weights<- c()
temp <- vector(length=length(times))
for (j in 1:11){
  weights<-sum(karnelshours[,j]) #weights for time j 
  temp[j]<- sum(predframehour[,12]*predframehour[,j])/ weights
}


#############combining all three kernels. 
finaltemp<-vector(length=11)
for ( i in 1:11){
  vikt<-sum(karnelshours[,i]+karnels+karnelstime)
  finaltemp[i]<-(sum(karnelshours[,i]*predframehour[,12]+karnels*predframehour[,12]+karnelstime*predframehour[,12]))/vikt
}
finaltemp
plot(finaltemp, type="o", xaxt="n", xlab="Time of day")
axis(1, at=1:11, labels=seq(04,24,2))

