## name (Carles Sans Fuentes)
## liuid (carsa564)

#######################Installations####################################

#install.packages("devtools")
#install.packages("testthat")
#devtools::install_github("MansMeg/markmyassignment")


#############################Setting path################################
library(markmyassignment)
lab_path <-
  "https://raw.githubusercontent.com/MansMeg/AdvRCourse/master/Labs/Tests/lab2.yml"
set_assignment(lab_path)
####################################################################################
### 1.1 Conditional statements

name<- "Carles Sans Fuentes"
liuid<- "carsa564"


##################################################
### 1.1.1 sheldon game(player1, player2)

sheldon_game <- function(player1, player2){
  players_choice<-c(player1, player2)
  possible_moves<-c("rock", "paper", "scissors", "lizard" , "spock")
  intersection1 <-c(intersect(possible_moves,player1))
  intersection2 <-c(intersect(possible_moves,player2))
  if(intersection1 != player1 |intersection2 != player2){
  stop()
  }
  #Draw
  else if (player1 == player2){ return("Draw!")} 
  #All possibilities
  else if (player1=="paper" & player2 =="rock") {return ("Player 1 wins!")}
  else if (player2=="paper" & player1 =="rock") {return ("Player 2 wins!")}
  else if (player1=="scissors" & player2 =="paper") {return ("Player 1 wins!")}
  else if (player2=="scissors" & player1 =="paper") {return ("Player 2 wins!")}
  else if (player1=="rock" & player2 =="lizard") {return ("Player 1 wins!")}
  else if (player2=="rock" & player1 =="lizard") {return ("Player 2 wins!")}
  else if (player1=="lizard" & player2 =="spock") {return ("Player 1 wins!")}
  else if (player2=="lizard" & player1 =="spock") {return ("Player 2 wins!")}
  else if (player1=="spock" & player2 =="scissors") {return ("Player 1 wins!")}
  else if (player2=="spock" & player1 =="scissors") {return ("Player 2 wins!")}
  else if (player1=="scissors" & player2 =="lizard") {return ("Player 1 wins!")}
  else if (player2=="scissors" & player1 =="lizard") {return ("Player 2 wins!")}
  else if (player1=="lizard" & player2 =="paper") {return ("Player 1 wins!")}
  else if (player2=="lizard" & player1 =="paper") {return ("Player 2 wins!")}
  else if (player1=="paper" & player2 =="spock") {return ("Player 1 wins!")}
  else if (player2=="paper" & player1 =="spock") {return ("Player 2 wins!")}
  else if (player1=="spock" & player2 =="rock") {return ("Player 1 wins!")}
  else if (player2=="spock" & player1 =="rock") {return ("Player 2 wins!")}
  else {return("error!")}
}

sheldon_game("lizard", "spock")
sheldon_game("rock", "paper")


####################################################################################
### 1.2 for loops
###1.2.1 my moving median()

my_moving_median <- function(x,n,...){
  stopifnot(is.numeric(x) &  is.numeric(n) & length(n)==1)
  result <- c()
  
  for (i in 1:(length(x)-n)){
   
     result[i] <- median(x[i:(i+n)],...)
    
   }
  
  result
}
my_moving_median(x = 1:10, n=2)
my_moving_median(x = 5:15, n=4)
my_moving_median(x = c(5,1,2,NA,2,5,6,8,9,9), n=2)
my_moving_median(x = c(5,1,2,NA,2,5,6,8,9,9), n=2, na.rm=TRUE)
#############################################################
 #### 1.2.2 for mult table()

for_mult_table <- function(from, to){
  ## Stop if it not numeric and scalar 
  stopifnot(is.numeric(from) & length(from)==1 &  is.numeric(to) & length(to)==1)
  ## Creating vector with the inputs
  inputs<-c(from:to)
  ## Creating a matrix with the appropiate dimensions
  mymat<- matrix(ncol=length(inputs),nrow=length(inputs))
  
  # Giving the proper names to the table
  colnames(mymat)<-from:to
  rownames(mymat)<-from:to
  
  #Putting the numbers in the matrix
  for(i in 1:length(from:to))  # for each row
  {
    for(j in 1:length(from:to)) # for each column 
    {
      mymat[i,j] = inputs[i]*inputs[j]
    }
  }
  mymat
  }

for_mult_table(from = 1, to = 5)
for_mult_table(from = 10, to = 12) ###
##################################################################################################
##1.3 while loops
  ##1.3.1 find cumsum()

find_cumsum <- function(x, find_sum){
  ## Stop if conditions are not met
  stopifnot(is.numeric(x) & is.vector(x) &  is.numeric(find_sum) & length(find_sum)==1)
  
  cumsum <- 0
  i <- 1
      while (cumsum < find_sum){
      if (length(x)==i-1){
        cumsum
      break
        }
      cumsum <- cumsum + x[i] 
    i <- i+1
    
  }
  cumsum
}

find_cumsum(x=1:100, find_sum=500)
find_cumsum(x=1:10, find_sum=1000)
##################################################################################################
##1.3.2 while mult table()

while_mult_table <- function(from, to){
  ## Stop if it not numeric and scalar 
  stopifnot(is.numeric(from) & length(from)==1 &  is.numeric(to) & length(to)==1)
  ## Creating vector with the inputs
  inputs<-c(from:to)
  ## Creating a matrix with the appropiate dimensions
  mymat<- matrix(ncol=length(inputs),nrow=length(inputs))

# Giving the proper names to the table
colnames(mymat)<-from:to
rownames(mymat)<-from:to
##Calculus
count_rows <- 1
while (count_rows < (length(from:to)+1)){
  count_col<-1
  
  while (count_col < (length(from:to)+1)){
    
    mymat[count_rows,count_col] <- inputs[count_rows]*inputs[count_col]
    count_col <- count_col + 1
    
    }
  count_rows <- count_rows + 1
}
mymat
}

while_mult_table(from = 3, to = 5)
while_mult_table(from = 7, to = 12)

##################################################################################################
##  1.4 repeat and loop controls
## 1.4.1 repeat find cumsum()

repeat_find_cumsum <- function(x, find_sum){
  ## Stop if conditions are not met
  stopifnot(is.numeric(x) & is.vector(x) &  is.numeric(find_sum) & length(find_sum)==1)
  cumsum <- 0
  i <- 1
  repeat {
    if (length(x)==i-1 |cumsum > find_sum){
      cumsum
      break
    }
   
    cumsum <- cumsum + x[i] 
    i <- i+1
    
  }
  cumsum
}

repeat_find_cumsum(x=1:100, find_sum=500)
repeat_find_cumsum(x=1:10, find_sum=1000)

########################################################################
##1.4.2 repeat my moving median()

repeat_my_moving_median <- function(x,n,...){
  stopifnot(is.numeric(x) &  is.numeric(n) & length(n)==1)
  result <- c()
  i<-1
  repeat {
    
    result[i] <- median(x[i:(i+n)],...)
    i<- i + 1
    if (i > (length(x)-n)){
      break
    }
  }
  
  result
}

###########################
repeat_my_moving_median(x = 1:10, n=2)
repeat_my_moving_median(x = 5:15, n=4)
repeat_my_moving_median(x = c(5,1,2,NA,2,5,6,8,9,9), n=2)

########################################################################
##1.5 Environment
## 1.5.1 in environment()

in_environment <- function(env){
  ls(env)
}

env <- search()[length(search())]
env
funs <- in_environment(env)
funs[1:5]


########################################################################
##1.6 Functionals
##1.6.1 cov()


cov <- function(X){
  stopifnot(is.data.frame(X))
  anonymous <- lapply(X, function(X)sd(X)/mean(X) )
  anonymous<- unlist(anonymous)
  anonymous
  }
data(iris)
cov(X = iris[1:4])
cov(X = iris[3:4])
########################################################################
##1.7 Closures
##1.7.1 moment()
moment <- function(i){
  stopifnot(is.numeric(i))
  function(x){
    stopifnot(is.numeric(x))
    mean((x-mean(x))**i)
    
  }
}

m1 <- moment(i=1)
m1(1:100)
m2 <- moment(i=2)
m2(1:100)
########################################################################


