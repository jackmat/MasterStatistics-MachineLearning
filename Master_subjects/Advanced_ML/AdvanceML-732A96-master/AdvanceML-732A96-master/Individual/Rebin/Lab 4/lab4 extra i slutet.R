######################
transModel <- function(sd = 1, z_t, z_p){
  (dnorm(z_t, mean = z_p , sd = sd)+
     dnorm(z_t, mean = z_p + 1 , sd = sd)+
     dnorm(z_t, mean = z_p + 2 , sd = sd))/3
}




zCorrect <- c()
xCorrect <- c()

zCorrect[1] <- init() # z_1
xCorrect[1] <- sEmissModel(z_t = zCorrect[1]) # x_1


for (i in 2:100){
  xCorrect[i] <- sEmissModel(sd = 1, z_t = zCorrect[i-1]) # x_t ges från z_t
  zCorrect[i] <- sTransModel(sd = 1, z_p = xCorrect[i]) # z_t från z_t-1
}

#now we remove z_t and only use obs. steps:

#1. Generate 100 Z_1 which contains 100 inital particles at time step 1 from a uniform dist. nr between 1 - 100
#2  We have X1 which is the first obs of xCorrect.
#3

# Process: Kör igenom W functionen för varje värde av Z där Z[1,] <- Init och resten simuleras från transtion.
#Emission steget som finns i vikten nedan kommer simuleras innan eftersom vi behöver vikterna för att simulera transition. där använder vi ett värde av x från xcorrect[i] det som är av intresse
# Så för första iterationen kommer vi använda i W functionen nedan x_t = xCorrect[1] och z_t = zInit men i andra iterationen använder ci xCorrect[2] och z_t[2,] som vi får från vår transitionmodell som vi kör in vikterna i


eSample <- function(){
  sample(1:3,1, replace = TRUE, prob = rep(1,3)/3)
}

tSample <- function(){
  sample(1:3,1,replace = TRUE , prob = rep(1,3)/3)
}

step <- 10
particles <- 100
zMat <- matrix(0, ncol = particles, nrow = steps)
zMat[1,] <- zInit
xMat <- matrix(0, ncol = particles, nrow = steps)
xMat[1,] <- xCorrect

W <- function(x_t, z_t){
  e <- emissModel(x_t = x_t, z_t = z_t)
  weights <- e/sum(e)
  return(weights)
}

funTest <- function(){
  step <- 4

  for(i in 2:step){
    xt <- xCorrect[i]
    wt <- W(x_t = xt, z_t = zMat[i-1,])

    sampleMeans <- sample(zMat[i-1,],
                          size = length(wt),
                          replace = TRUE, prob = wt)
    zMat[i,] <- function(prev = zMat[i-1,], sampleMeans = sampleMeans){

      return(transModel(z_t = sampleMeans, z_p = prev))
    }
  }
  return("Z" = zMat, "X")
}





emissModel(x_t = xCorrect[1], z_t = zInit)











transModel(sd = 1, z_t = zInit, z_p = zInit)

particleFilter <- function(step, particles, sd, init){



}
