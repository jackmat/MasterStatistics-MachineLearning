#install.packages("stringr")
#install.packages("plyr")
#install.packages("stats")
#install.packages("arules")
#install.packages("arulesViz")
#install.packages("stringr")
library(plyr)
library(stats)
library(arulesViz)
library(arules)
library(stringr)

###Reading data set

data<- read.csv("C:/Users/Carles/Desktop/Data mining/dataset.1.txt",sep =",", stringsAsFactors = FALSE)
class(data)
table(data[,1])

##############################################################################################################
####################################CLEANING DATA#############################################################
##############################################################################################################

colnames(data)<-c("Age", "workclass", "fnlwgt", "education","education-num", "marital-status", "occupation", "relationship", "race", "sex","capitalgain", "capitalloss", "hoursperweek", "nativeCountry", "Wining50K")

##Droping education num because it stands for the same as education but continuous
drops<- c("education-num")

data<- data[ , !(names(data) %in% drops)]

table(data[["Wining50K"]])
### <=50K   >50K 
### 24719   7841 


##wining +50K == to 1, less than 50k equals to 0

levels(data[["Wining50K"]])<-c(0,1)

#Data does not contain NA
apply(data, 2, anyNA)


##########################################A PRIORI#################################################
#install.packages("arules")



#colnames(data)<-c("Age", "workclass", "fnlwgt", "education","education-num", "marital-status", "occupation", "relationship", "race", "sex","capitalgain", "capitalloss", "hoursperweek", "nativeCountry", "Wining50K")

min(data[,1]); max(data[,1]);
hist(data[,1])
thirties<-which(data[,1]<30)
fourties<-which((data[,1]>=30)&(data[,1]<40))
fifties<-which((data[,1]>=40)&(data[,1]<50))
sixties<-which((data[,1]>=50)&(data[,1]<65))
pensioners<- which(data[,1]>=65)

data[thirties,1]<-"<30s"
data[fourties,1]<-"30-39s"
data[fifties,1]<-"40-49s"
data[sixties,1]<-"50-64s"
data[pensioners,1]<-">65"

table(data[,1])
      ### <30s    >65 30-39s 40-49s 50-64s 
      ### 9711   1336   8612   7175   5726 

colnames(data)[c(3,5,11,13)]##[1] "fnlwgt" "education-num" "capitalgain"   "hoursperweek" 

#### Final weight: The \# of people the census takers believe that observation represents. We will be ignoring this variable
hist(data[,3])
min(data[,3]); max(data[,3]);

belowhundred<- which(data[,3]<100000)
hundred<- which((data[,3]>=100000)&(data[,3]<200000))
twohundred<- which((data[,3]>=200000)&(data[,3]<300000))
threehundred<-which((data[,3]>=300000)&(data[,3]<400000))
fourhundred<- which(data[,3]>=400000)

data[belowhundred,3]<-"<100M"
data[hundred,3]<-"100-200M"
data[twohundred,3]<-"200-300M"
data[threehundred,3]<-"300-400M"
data[fourhundred,3]<-">400M"

table(data[,3])
class(data[,3])


#### "capital gain"

hist(data[,10])
min(data[,10]); max(data[,10]);
NoGain<-which(data[,10] == 0)
Gain<-which(data[,10]>1)
data[NoGain,10]<-"No Gain"
data[Gain,10]<-" Gain"
table(data[,10])

#### "capital loss"

hist(data[,11])
min(data[,11]); max(data[,11]);
NoLoss<- which(data[,11]==0)
Loss<- which(data[,11]>0)
data[NoLoss,11]<-"No Loss"
data[Loss,11]<-" Loss"

table(data[,11])
#### "hoursperweek"
min(data[,12]); max(data[,12]);
hist(data[,12])
lesstwenty<- which(data[,12]<20)
twenty<- which((data[,12]>=20)&(data[,12]<35))
thirtyfive<-which((data[,12]>=35)&(data[,12]<45))
fortyfive<-which((data[,12]>=45)&(data[,12]<60))
PlusSixty<-which(data[,12]>=60)

data[lesstwenty,12]<-"<20 hours"
data[twenty,12]<-"20-34 hours"
data[thirtyfive,12]<-"35-44 hours"
data[fortyfive,12]<-"45-60 hours"
data[PlusSixty,12]<-">60 hours"

table(data[,12])

Myframe<- as.data.frame(data)


DataFactor<-Myframe
for(i in 1:ncol(data)){
  #levels(data[,i])<-names(table(data[,i]))
  DataFactor[,i]<- as.factor(data[,i])
}
##############################################################################################################
####################################A PRIORI ALGORITHM#############################################################
##############################################################################################################

####MAIN FUNCTIONS TO CHECK FROM THE RULES STATED, which ones are the repeated variables most variables 
MainWordsList<-function(data= resultswin50_global , start=2, stop=22, numbercommas=6){
  require(stringr)
  ####just taking into account those which has more than 5 variables, counted as comma separated
  data<-data[which(str_count(data[,1], ',')>=numbercommas),]
  data[,1]<- as.character(data[,1])
  for(i in 1:length(data[,1])){
    ##Getting rid of last variable and first variable for counting most important features
    data[i,1]<- substr(data[i,1],start,nchar(data[i,1])-stop)
  }
  
  featWin50<-table(unlist(strsplit(as.character(data[,1]), split = c(","))))
  return(featWin50)
}

##### RULES FOR WIN +50K###################
Mytransmatrix <- as(DataFactor, "transactions")

Win50<- apriori(Mytransmatrix, parameter = list(supp = 0.02, conf = 0.5, target = "rules"),
        appearance = list(rhs = c("Wining50K= >50K"), 
                          default="lhs"))
plot(Win50, method = NULL, measure = "support", shading = "lift",
     interactive = FALSE, data = NULL, control = NULL)

rules_subsetWin50 <- subset(Win50, subset=(lift>1.05& confidence>0.7)) 
plot(rules_subsetWin50, method = NULL, measure = "support", shading = "lift",
     interactive = FALSE, data = NULL, control = NULL)
##larger by support
inspect(head(rules_subsetWin50, n = 10, by= "support"))

##larger by confidence
inspect(head(rules_subsetWin50, n = 10, by= "confidence"))

resultswin50_global<- as(Win50, "data.frame");
resultswin50_subset<- as(rules_subsetWin50, "data.frame");

featWin50_subset<- sort(MainWordsList(resultswin50_subset,2,22,1))
featWin50_global <- sort(MainWordsList(resultswin50_global,2,22,1))
##### RULES FOR WIN -50K###################

NoWin50<- apriori(Mytransmatrix, parameter = list(supp = 0.10, conf = 0.7, target = "rules"),
                  appearance = list(rhs = c("Wining50K= <=50K"), 
                                    default="lhs"))

plot(NoWin50, method = NULL, measure = "support", shading = "lift",
     interactive = FALSE, data = NULL, control = NULL)

rules_subsetNoWin50 <- subset(NoWin50, subset=(lift>1.05& confidence>0.7)) 
plot(rules_subsetNoWin50, method = NULL, measure = "support", shading = "lift",
     interactive = FALSE, data = NULL, control = NULL)
##larger by support
inspect(head(rules_subsetNoWin50, n = 10, by= "support"))

##larger by confidence
inspect(head(rules_subsetNoWin50, n = 10, by= "confidence"))

resultsNowin50_global<- as(NoWin50, "data.frame");
resultsNowin50_subset<- as(rules_subsetNoWin50, "data.frame");
dim(resultsNowin50_subset)
featNoWin50_subset<- sort(MainWordsList(resultsNowin50_subset,2,22,1))
featNoWin50_global<- sort(MainWordsList(resultsNowin50_global,2,22,1))


table(data[,8])
##### RULES FOR WIN -50K###################
###Rules for female
NoWin50Female<- apriori(Mytransmatrix, parameter = list(supp = 0.10, conf = 0.7, target = "rules"),
                  appearance = list(rhs = c("Wining50K= <=50K"),
                                    items = c("sex= Female"),
                                    default="lhs"))
rules_subsetWoman <- subset(NoWin50Female, (lhs %in% "sex= Female"))
inspect(head(rules_subsetWoman, n = 10, by= "confidence"))
inspect(head(rules_subsetWoman, n = 10, by= "support"))

table(data[,9])[1]/sum(table(data[,9]))
###   Female 
###  0.3308047


###Rules for female to win 50K
Win50Female<- apriori(Mytransmatrix, parameter = list(supp = 0.02, conf = 0.2, target = "rules"),
                        appearance = list(rhs = c("Wining50K= >50K"),
                                          items = c("sex= Female"),
                                          default="lhs"))
rules_subsetWomanwin50 <- subset(Win50Female, (lhs %in% "sex= Female"))
inspect(head(rules_subsetWomanwin50, n = 10, by= "confidence"))
inspect(head(rules_subsetWomanwin50, n = 10, by= "support"))

table(data[,9])[1]/sum(table(data[,9]))
###   Female 
###  0.3308047


###Rules for race black
NoWin50Black <-apriori(Mytransmatrix, parameter = list(supp = 0.03, conf = 0.5, target = "rules"),
                      appearance = list(rhs = c("Wining50K= <=50K"),
                                        items = c("race= Black"),
                                        default="lhs"))
rules_subsetBlack <- subset(NoWin50Black, (lhs %in% "race= Black"))
inspect(head(rules_subsetBlack, n = 10, by= "confidence"))
inspect(head(rules_subsetBlack, n = 10, by= "support"))

table(data[,8])[3]/sum(table(data[,8]))
###    Black 
####0.09594595 


###Rules for race black to win 50K
Win50Black <-apriori(Mytransmatrix, parameter = list(supp = 0.005, conf = 0.2, target = "rules"),
                       appearance = list(rhs = c("Wining50K= >50K"),
                                         items = c("race= Black"),
                                         default="lhs"))
rules_subsetBlackwin <- subset(Win50Black, (lhs %in% "race= Black"))
inspect(head(rules_subsetBlackwin, n = 10, by= "confidence"))
inspect(head(rules_subsetBlackwin, n = 10, by= "support"))

table(data[,8])[3]/sum(table(data[,8]))
###    Black 
####0.09594595 