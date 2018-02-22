library(calibrate)
library(yacca)
data(heads)

## other functions stats::cancor, CCA::cc do not have the option 
## xscale, yscale, directly i.e.
## bring to unit variance - we deal with the correlation matrix
## we would manually have to rescale
## vegan::cca has this as an option

CCres<-cca(heads[,1:2],heads[,3:4],xscale=TRUE,yscale=TRUE)

## in $xcoef, $ycoef the results are scaled so that U1, V1, U2, V2 have unit variance
## here we rescale them for illustration to correspond to 
## unit length eigenvectors of solve(R11)%*%R12%*%solve(R22)%*%R21

a1<-CCres$xcoef[,1]/(sqrt(sum(CCres$xcoef[,1]^2)))
a2<-CCres$xcoef[,2]/(sqrt(sum(CCres$xcoef[,2]^2)))
b1<-CCres$ycoef[,1]/(sqrt(sum(CCres$ycoef[,1]^2)))
b2<-CCres$ycoef[,1]/(sqrt(sum(CCres$ycoef[,2]^2)))

yacca::plot.cca(CCres)

