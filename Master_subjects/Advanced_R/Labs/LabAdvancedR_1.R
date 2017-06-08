## name (Carles Sans Fuentes)
## liuid (carsa564)

#######################Installations####################################

#install.packages("devtools")
#install.packages("testthat")
#devtools::install_github("MansMeg/markmyassignment")


#############################Setting path################################
library(markmyassignment)
lab_path <-
  "https://raw.githubusercontent.com/MansMeg/AdvRCourse/master/Labs/Tests/lab1.yml"
set_assignment(lab_path)
##########################################################################
### 1.1 Vectors

name<- "Carles Sans Fuentes"
liuid<- "carsa564"

##############################################################################################################
  ### 1.1.1 my num vector()#######################################################

my_num_vector<- function(){
  a<-log10(11)
  b<-cos(pi/5)
  c<-exp(pi/3)
  d<-(1173%%7)/19
  f<-c(a,b,c,d)
  return(f)
}
my_num_vector()
##############################################################################################################
### assignment 1.1.2

filter_my_vector<-function(x,leq){
x[x>=leq]=NA
x
}
filter_my_vector(x = c(2, 9, 2, 4, 102), leq = 4)
##############################################################################################################
### assignment 1.1.3

dot_prod<-function(a,b){
  y<-sum(a*b)
  y
}

dot_prod(a = c(3,1,12,2,4), b = c(1,2,3,4,5))
dot_prod(a = c(-1,3), b = c(-3,-1))
##############################################################################################################
### assignment 1.1.4

approx_e <-function(N){
  e <-sum(1/factorial(0:N))
  e
  }

approx_e(N = 2)
approx_e(N = 4)
exp(1)
###Test how large N need to be to approximate e to the fifth decimal place.

N=1
while((round(approx_e(N),digits = 5)<round(exp(1),digits = 5))){
  N=N+1
  print(N)
}

############################################################################################################################################################################################################################
###1.2 Matrices
############################################################################################################## 
 ###1.2.1 my magic matrix()

my_magic_matrix<- function(){
  matrix(c(4,3,8,9,5,1,2,7,6),nrow=3)
}
my_magic_matrix()
##############################################################################################################
  ###1.2.2 calculate elements(A)

calculate_elements<- function(A){
  length(A)
  }

mat <- my_magic_matrix()
calculate_elements(A = mat)
new_mat <- cbind(mat, mat)
calculate_elements(A = new_mat)
##############################################################################################################
  ###1.2.3 row to zero(A, i)
  
row_to_zero <- function(A,i){
  A[i,]=0
  A
}

mat <- my_magic_matrix()
row_to_zero(A = mat, i = 3)
##############################################################################################################
  ###1.2.4 add elements to matrix(A, x, i, j)
  
add_elements_to_matrix<- function(A,x,i,j){
  A[i,j]=A[i,j]+x
  A
}

mat <- my_magic_matrix()
add_elements_to_matrix(A = mat, x = 10, i = 2, j = 3)
add_elements_to_matrix(A = mat, x = -2, i = 1:3, j = 2:3)
##############################################################################################################
###1.3 Lists
##############################################################################################################
  ###1.3.1 my magic list()
my_magic_list<- function(){
  listado<-list(info="my own list",my_num_vector(), my_magic_matrix())
  listado
}
my_magic_list()


##############################################################################################################
  ###1.3.2 change info(x, text)

change_info<-function(x,text){
  x$info=text
  x
}
a_list <- my_magic_list()
change_info(x = a_list, text = "Some new info")
##############################################################################################################
  ###1.3.3 add note(x, note)
add_note<- function(x , note){
    
  y<-append(x,note, after=length(x))
  names(y)[length(y)]<-"note"
  y 

  }

b_list <- my_magic_list()
add_note(x = b_list, note = "This is a magic list!")

##############################################################################################################
  ###1.3.4 sum numeric parts(x)

sum_numeric_parts <- function(x){
    sum(as.numeric(unlist(x)),na.rm=TRUE)
    }

a_list <- my_magic_list()
sum_numeric_parts(x = a_list)
sum_numeric_parts(x = a_list[2])

##############################################################################################################
###1.4 data.frames
##############################################################################################################
  ###1.4.1 my data.frame()

my_data.frame <-function(){
  id <-c(1,2,3)
  name <-c("John", "Lisa", "Azra")
  income <-c(7.30,0.00,15.21)
  rich <-c(FALSE, FALSE, TRUE)
  data.frame(id, name, income, rich)
  
}
my_data.frame()
##############################################################################################################
  ###1.4.2 sort head(df, var.name, n)

sort_head <- function(df, var.name, n){
###head(df[order(df$var.name, decreasing=TRUE),],n)----> Why does not it work with symbol $?
  variable<-df[var.name]
  sortedby<-df[order(variable, decreasing=TRUE),]
  head(sortedby, n)
  }
###head(iris[order(iris$Petal.Length, decreasing=TRUE),],5)
data(iris)
sort_head(df = iris, var.name = "Petal.Length", n = 5)

##############################################################################################################
  ###1.4.3 add median variable(df, j)
add_median_variable <- function(df, j){
  compared_to_median <- c(rep(NA, length(df[,j])))
  database<-cbind(df,compared_to_median)
  database.length<-length(database)
  median <- median(database[,j])
  for(i in 1:length(database[,j])){
    if(database[i,j] == median){
      database[i,database.length] <- "Median"
    }
    else if (database[i,j] < median){
      database[i,database.length] <- "Smaller"
    }
    
    else if (database[i,j] > median){
      database[i,database.length] <- "Greater"
    }
  }
  database
  
}

data(faithful)
head(add_median_variable(df = faithful, 1))
tail(add_median_variable(df = faithful, 2))

##############################################################################################################
  ###1.4.4 analyze columns(df, j)

analyze_columns <- function(df, j){
  a<-unlist(j)
  first <- a[1]
  second <-a[2]
  lst1<-c(mean=mean(df[,first]), median=median(df[,first]), sd=sd(df[,first]))
  lst2<-c(mean=mean(df[,second]), median=median(df[,second]), sd=sd(df[,second]))
  correlation_matrix<-cor(df[,j])
  join_list<-list(lst1,lst2,correlation_matrix)
  names(join_list)<-c(colnames(df)[first],colnames(df)[second],"correlation_matrix")
  
  join_list
}  

    
data(faithful)
analyze_columns(df = faithful, 1:2)

data(iris)
analyze_columns(df = iris, c(1,3))

analyze_columns(df = iris, c(4,1))


#######################################################################

  

