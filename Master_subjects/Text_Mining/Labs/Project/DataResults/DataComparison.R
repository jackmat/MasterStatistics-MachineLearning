############Comparison of data
storePath = "C:/Users/Carles/Desktop/MasterStatistics-MachineLearning/Master_subjects/Text_Mining/Labs/Project/DataResults/"


# EmailList<-read.table(paste0(storePath,"EmailList.txt"), sep = "\t")
# EmailList<- lapply(EmailList, as.character)
# BestRecom<-read.table(paste0(storePath,"BestReccommendations.txt"), sep = ",")
ParamOpt<-read.table(paste0(storePath,"ParamOpt.txt"), sep = ",")
ParamRange<-read.table(paste0(storePath,"ParamRange.txt"), sep = ",")

library(dplyr)
library(tidyr)
library(ggplot2)
theme_set(theme_bw())  # pre-set the bw theme.

mydata<- data.frame(x = ParamRange, y = ParamOpt)
colnames(mydata) <- c("x","y")


mydata %>% ggplot(aes(x = x, y = y))+
  geom_line()+
  geom_point(aes(x = x[which.max(y)], y =y[which.max(y)]), col = "red")+
  labs(subtitle="parameter variable optimization", 
       y="% equal to Gold Standard classif. (from 0-1)", 
       x="parameter values taken", 
       title="Optimization of the CV Inf. Retr. by important words")

  



