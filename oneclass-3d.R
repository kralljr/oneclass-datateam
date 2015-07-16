# source class1 function
source("oneclass-pred.R")


# Load libraries
library(e1071)
library(dplyr)
library(mvtnorm)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(devtools)
library(plot3D)

# May need to run this:
#install_github("ropensci/plotly")
library(plotly)

load("preds_3d.RData")


##################
# Get grid to get boundary
# Sample grid points near boundary
N <- 400
x1 <- unique(xs[, 1])
x2 <- unique(xs[, 2])
newdat <- as.matrix(newdat)
genX <- class1(newdat, "radial", svm1 = svm_model, surf = T)
genXT <- class1(as.matrix(newdatxT), "radial", svm1 = svm_model, surf = T)
genA <- class1(as.matrix(newdat2), "radial", svm1 = svm_model, surf = T)

genB <- class1(as.matrix(b1), "radial", svm1 = svm_model, surf = T)
xs <- xs[order(xs[, 1], xs[, 2]), ]
z <- matrix(classT[[2]], byrow = T, nrow = length(x1))
seqs <- seq(1, N, by = 10)


plotfun3d <- function(phi1, theta1) {

cex1 <- 1.5
lwd1 <- 3
p1 <- persp(x1[seqs], x2[seqs], z[seqs, seqs],  phi = phi1, theta = theta1, border = "grey70", xlab = "Feature 1", ylab = "Feature 2", zlab = "Kernel value")
points(trans3D(newdat[, 1], newdat[, 2], genX[[2]], pmat = p1), cex = cex1) 

points(trans3D(newdat2[, 1], newdat2[, 2], genA[[2]], pmat = p1), col = cols[2], pch = "*", cex = cex1) 

points(trans3D(newdatxT[, 1], newdatxT[, 2], genXT[[2]], pmat = p1), col = cols[2], cex = cex1) 
lines(trans3D(b1[, 1], b1[, 2], genB[[2]], pmat = p1), col = cols[1], lwd = lwd1)

}
