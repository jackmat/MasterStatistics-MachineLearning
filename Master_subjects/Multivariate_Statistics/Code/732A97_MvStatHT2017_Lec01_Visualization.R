# Visualization code based on 
# An Introduction to Applied Multivariate Analysis with R, Authors: Everitt, Brian, Hothorn, Torsten

library(MVA) # library attached to the book
library(lattice) # for 3D plots
library(KernSmooth) ## for scatterplot with contours
library(aplpack) # for Chernoff faces

## code based on demos in MVA

## The data
###################################################
### code chunk number 2: ch:Viz:data
###################################################
measure <- 
structure(list(V1 = 1:20, V2 = c(34L, 37L, 38L, 36L, 38L, 43L,
40L, 38L, 40L, 41L, 36L, 36L, 34L, 33L, 36L, 37L, 34L, 36L, 38L,
35L), V3 = c(30L, 32L, 30L, 33L, 29L, 32L, 33L, 30L, 30L, 32L,
24L, 25L, 24L, 22L, 26L, 26L, 25L, 26L, 28L, 23L), V4 = c(32L,
37L, 36L, 39L, 33L, 38L, 42L, 40L, 37L, 39L, 35L, 37L, 37L, 34L,
38L, 37L, 38L, 37L, 40L, 35L)), .Names = c("V1", "V2", "V3",
"V4"), class = "data.frame", row.names = c(NA, -20L))
measure <- measure[,-1]
names(measure) <- c("chest", "waist", "hips")
measure$gender <- gl(2, 10)
levels(measure$gender) <- c("male", "female")

data("USairpollution", package = "HSAUR2")



?USairpollution
mlab <- "Manufacturing enterprises with 20 or more workers"
plab <- "Population size (1970 census) in thousands"


###################################################
### code chunk number 5: ch:Viz:USairpollution:plot1
###################################################
plot(popul ~ manu, data = USairpollution, xlab = mlab, ylab = plab,pch=19,cex=1)


###################################################
### code chunk number 7: ch:Viz:USairpollution:plot2
###### scatterplot with ticks
###################################################
plot(popul ~ manu, data = USairpollution, xlab = mlab, ylab = plab,pch=19,cex=1)
rug(USairpollution$manu, side = 1)
rug(USairpollution$popul, side = 2)


###################################################
### code chunk number 6: ch:Viz:USairpollution:plot3setup
##### marginal distributions are shown by boxplot and histogram
###################################################

layout(matrix(c(2, 0, 1, 3), nrow = 2, byrow = TRUE),
       widths = c(2, 1), heights = c(1, 2), respect = TRUE)
xlim <- with(USairpollution, range(manu)) * 1.1
plot(popul ~ manu, data = USairpollution, cex.lab = 0.9,
     xlab = mlab, ylab = plab, type = "n", xlim = xlim)
with(USairpollution, text(manu, popul, cex = 0.6,
     labels = abbreviate(row.names(USairpollution))))
with(USairpollution, hist(manu, main = "", xlim = xlim))
with(USairpollution, boxplot(popul))


###################################################
### code chunk number 9: ch:Viz:USairpollution:plot4
##### scatterplot with bivariate boxplot
###################################################
outcity <- match(lab <- c("Chicago", "Detroit",  "Cleveland", "Philadelphia"), rownames(USairpollution))
x <- USairpollution[, c("manu", "popul")]
bvbox(x, mtitle = "", xlab = mlab, ylab = plab, pch=19, cex=1)
text(x$manu[outcity], x$popul[outcity], labels = lab,cex = 0.7, pos = c(2, 2, 4, 2, 2))

###################################################
### code chunk number 11: ch:Viz:USairpollution:chull
##### plot with convex hull of bivariate data
###################################################
(hull <- with(USairpollution, chull(manu, popul)))
###################################################
### code chunk number 12: ch:Viz:USairpollution:chullplot
###################################################
with(USairpollution, 
     plot(manu, popul, pch = 1, xlab = mlab, ylab = plab))
with(USairpollution, 
     polygon(manu[hull], popul[hull], density = 15, angle = 30))


###################################################
### code chunk number 16: ch:Viz:USairpollution:plot5
### bubble plot, bubble is the third variable
###################################################
ylim <- with(USairpollution, range(wind)) * c(0.95, 1)
plot(wind ~ temp, data = USairpollution, 
     xlab = "Average annual temperature (Fahrenheit)",
     ylab = "Average annual wind speed (m.p.h.)", pch = 10,
     ylim = ylim)
with(USairpollution, symbols(temp, wind, circles = SO2, 
                             inches = 0.5, add = TRUE))

###################################################
### code chunk number 17: ch:Viz:USairpollution:plot6
#### points with stars attached
###################################################
plot(wind ~ temp, data = USairpollution,
     xlab = "Average annual temperature (Fahrenheit)",
     ylab = "Average annual wind speed (m.p.h.)", pch = 10,
     ylim = ylim)
with(USairpollution,
    stars(USairpollution[,-c(2,5)], locations = cbind(temp, wind),
          labels = NULL, add = TRUE, cex = 0.5))


###################################################
### code chunk number 19: ch:Viz:USairpollution:plot8
#### scatterplot with all pairs of variables
###################################################
pairs(USairpollution, pch = ".", cex = 1.5)


###################################################
### code chunk number 20: ch:Viz:USairpollution:plot9
#### scatterplot with all pairs of variables and linear relationship
###################################################
pairs(USairpollution, 
      panel = function (x, y, ...) {
          points(x, y, ...)
          abline(lm(y ~ x), col = "grey")
      }, pch = ".", cex = 1.5)
      
      
###################################################
### code chunk number 28: ch:Viz-epakernel-fig
#### density function in 3D
###################################################
epa <- function(x, y) 
    ((x^2 + y^2) < 1) * 2/pi * (1 - x^2 - y^2)
x <- seq(from = -1.1, to = 1.1, by = 0.05)
epavals <- sapply(x, function(a) epa(a, x))
persp(x = x, y = x, z = epavals, xlab = "x", ylab = "y", 
      zlab = expression(K(x, y)), theta = -35, axes = TRUE, 
      box = TRUE)

###################################################
### code chunk number 30: ch:Viz:CYGOB1:plot1
#### scatterplot with contours
###################################################
CYGOB1d <- bkde2D(CYGOB1, bandwidth = sapply(CYGOB1, dpik))
plot(CYGOB1, xlab = "log surface temperature",
             ylab = "log light intensity",pch=19,cex=1)
contour(x = CYGOB1d$x1, y = CYGOB1d$x2, 
        z = CYGOB1d$fhat, add = TRUE)
###################################################
### code chunk number 31: ch:Viz:CYGOB1:plot2
###### and in 3D density
###################################################
persp(x = CYGOB1d$x1, y = CYGOB1d$x2, z = CYGOB1d$fhat,
      xlab = "log surface temperature",
      ylab = "log light intensity",
      zlab = "density")


###################################################
### code chunk number 33: ch:Viz:measure:plot1
##### contours with histograms
### data: measurements on chest, waist, hips for men and women
###################################################
panel.hist <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col = "grey", ...)
}
pairs(measure[, c("chest", "waist", "hips")],
      diag.panel = panel.hist,
      panel = function (x,y) {
          data <- data.frame(cbind(x,y))
          par(new = TRUE)
          den <- bkde2D(data, bandwidth = sapply(data, dpik))
          contour(x = den$x1, y = den$x2, 
                  z = den$fhat, axes = FALSE)
      })

###################################################
### code chunk number 34: ch:Viz:measure:plot2
##### 3D scatterplot
###################################################
library("scatterplot3d")
with(measure, scatterplot3d(chest, waist, hips,
     pch = (1:2)[gender], type = "h", angle = 55))

###################################################
### code chunk number 41: ch:Viz:USairpollution:stalac
##### staccalite plot to detect outliers
###################################################
stalac(USairpollution)


## =================================

### stars (in graphics)
## only statsm in all directions
?mtcars
stars(mtcars[, 1:7], key.loc = c(14, 2), main = "Motor Trend Cars : stars(*, full = F)", full = FALSE)
stars(USairpollution, cex = 0.55)


### Chernoff faces
data(longley) #A macroeconomic data set which provides a well-known example for a  highly collinear regression.
?longley
faces(longley[1:9,],face.type=0)
faces(longley[1:9,],face.type=1) ## with colour
