## https://en.wikipedia.org/wiki/European_water_vole
### R code from vignette source 'Ch-MDS.Rnw'
### Encoding: UTF-8
## R code from the MVA package, concerning Chapter 4 Multidimensional Scaling

library(ape)
library(HSAUR2)

###################################################
### code chunk number 20: MDS-watervoles-tab
###################################################
data("watervoles", package = "HSAUR2")
tmp <- watervoles
colnames(watervoles) <- abbreviate(colnames(watervoles))
watervoles <- as.data.frame(watervoles)
htab <- HSAURtable(watervoles)
htab$data[upper.tri(htab$data)] <- " "
toLatex(htab, pcol = 1, xname = "watervoles",
    caption = "Water voles data-dissimilarity matrix.",
    label = "MDS-watervoles-tab",
    rownames = TRUE)
watervoles <- tmp


###################################################
### code chunk number 21: MDS-voles-cmdscale
###################################################
data("watervoles", package = "HSAUR2")
voles_mds <- cmdscale(watervoles, k = 13, eig = TRUE)
voles_mds$eig


###################################################
### code chunk number 22: MDS-voles-criterion1
###################################################
cumsum(abs(voles_mds$eig))/sum(abs(voles_mds$eig))


###################################################
### code chunk number 23: MDS-voles-criterion2
###################################################
cumsum((voles_mds$eig)^2)/sum((voles_mds$eig)^2)


###################################################
### code chunk number 24: MDS-watervoles-plot
###################################################
x <- voles_mds$points[,1]
y <- voles_mds$points[,2]
plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2",
     xlim = range(x)*1.2, type = "n")
text(x, y, labels = colnames(watervoles), cex = 0.7)


###################################################
### code chunk number 25: MDS-watervoles-mst
###################################################
st <- mst(watervoles)
plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2",
     xlim = range(x)*1.2, type = "n")
for (i in 1:nrow(watervoles)) {
    w1 <- which(st[i, ] == 1)
    segments(x[i], y[i], x[w1], y[w1])
}
text(x, y, labels = colnames(watervoles), cex = 0.7)


