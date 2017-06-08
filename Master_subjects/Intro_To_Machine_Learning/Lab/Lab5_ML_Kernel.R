###LAb 5

set.seed(1234567890)
#install.packages("geosphere")
library(geosphere)
stations <- read.csv("C:/Users/M/Desktop/Statistics and Data Mining Master/Semester 1/Second part of the semester/intro to machine learning/lab5/stations.csv")
temps <- read.csv("C:/Users/M/Desktop/Statistics and Data Mining Master/Semester 1/Second part of the semester/intro to machine learning/lab5/temps50k.csv")
st <- merge(stations,temps,by="station_number")

###################################


date <- "2013-11-04" # The date to predict (up to the students)

a <- 58.4274 # The point to predict (up to the students)
b <- 14.826
times <- c("04:00:00", "06:00:00", "08:00:00","10:00:00",
           "12:00:00","14:00:00", "16:00:00", "18:00:00",
           "20:00:00","22:00:00","24:00:00")


AverageTemperature<- function(lon= a, lat=b, day_measure="2013-11-04", time_interest= times){
#################Different functions###############  
  temp <- vector(length=length(times))
    
      
  K_distance<- function(lon= a, lat=b){
    h_distance<-integer(nrow(st))  
    for(i in 1:nrow(st)){      
      h_distance[i] <- distHaversine(p1=c(a,b) ,p2=c(st[i,5],st[i,4]))/10000# Swedish miles
                   
    }
    
    ##Gaussian Kernel with data valued at 1
      h_distance_divh<- exp(-abs((h_distance)**2/(2*var(h_distance))))    # 
  
    #Distance frame
    
    return(h_distance_divh)
    }
  result_dist<- K_distance(lon=lon, lat=lat) 
  kernel_dist<- sum((as.data.frame(result_dist)*st[,11])/sum(result_dist))
  
  ######################################  
  
    h_date <- function(day_measure ){
      
    day_diff <- abs(as.Date(st[,9],format ="%Y-%m-%d") - as.Date(day_measure, format ="%Y-%m-%d"))
    
    ##Gaussian Kernel
    kernel_res<- exp((-abs(as.numeric(day_diff))**2)/(2*var(as.numeric(day_diff)/h)))
   
      
    ##Creating output with station reference
    return(kernel_res)
    
    }
    
    result_date<- h_date(day_measure = day_measure) 
    kernel_date<- sum((as.data.frame(result_date)*st[,11])/sum(result_date))
    ######################################    
  
    
    h_time <- function(time_interest){
      mycol<-as.difftime(as.character(st[,10]), units ="mins")##vector difftime
      
      time_diff<-matrix(NA,nrow =nrow(st), ncol =length(time_interest))# Setting the matrix for the time_interest_vector  
      kernel_res<-matrix(NA,nrow =nrow(st), ncol =length(time_interest))
      time_interest_diff<-as.difftime(time_interest, units = "mins" )
      
      for(i in 1:nrow(time_diff)){
        for(j in 1:length(time_interest)){      
          time_diff[i,j] <- (time_interest_diff[j] - mycol[i])
        }}
      
      for(j in 1:length(time_interest)){
      kernel_res[,j] <- exp(-abs(as.numeric(time_diff[,j]))**2/(2*var(as.numeric(time_diff[,j]))))}
      return(kernel_res)
      
    }
    result_time<- h_time(time_interest = time_interest)
  
    kernel_time<- integer(ncol(result_time))
    for( i in 1:ncol(result_time)){
    kernel_time[i]<- sum(result_time[,i]*st[,11]/sum(result_time[,i]))
    }
    
  ##sum(dO the kernel one per the tempertatures of one)/totalsum of temperatures
  for(i in 1:length(temp)){
    temp[i]<- 1/3*kernel_time[i]+1/3*kernel_date+1/3*kernel_dist
  }
  # Students' code here
  library(ggplot2)
  myplot<-  ggplot2::ggplot(data.frame(temp =temp))+
            geom_line(aes(x = seq(4,24,2), y = temp))+
            geom_point(aes(x = seq(4,24,2), y = temp))+
            xlab("time hours on the day")+
            ylab("temperature in ºC")+
            xlim(0,24)+
            ggtitle(paste0("Temperature by hours on the ", day_measure))+
            theme_dark()
  res<-list()
  res[["plot"]]<-myplot
  res[["average temperature"]]<- temp
  res[["temperature by kernel"]]<- data.frame(kernel_time=kernel_time,
                                              kernel_date= kernel_date, 
                                              kernel_dist= kernel_dist)
  return(res)

}
##Calling function
result<-AverageTemperature(lon= a, lat=b, day_measure="2013-11-04", time_interest= times)
  