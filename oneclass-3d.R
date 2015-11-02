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
load("gfigs.RData")




##################
# Get grid to get boundary
# Sample grid points near boundary
N <- 400
x1 <- unique(xs[, 1])
x2 <- unique(xs[, 2])

# Get kernels for all data
genX <- class1(as.matrix(newdat), "radial", svm1 = svm_model, surf = T)
genXT <- class1(as.matrix(newdatxT), "radial", svm1 = svm_model, surf = T)
genA <- class1(as.matrix(newdat2), "radial", svm1 = svm_model, surf = T)
genB <- class1(as.matrix(b1), "radial", svm1 = svm_model, surf = T)



# Set up data for persp function
xs <- xs[order(xs[, 1], xs[, 2]), ]
z <- matrix(classT[[2]], byrow = T, nrow = length(x1))
seqs <- seq(1, N, by = 10)

# Specify plot info
cex1 <- 2
lwd1 <-4
# Specify colors
cols <- brewer.pal(3, "Dark2")[1 : 2]




#' Function to plot decision boundary surface
#'
#' @param phi1 rotation 1
#' @param theta1 rotation 2
#' @param zlab label for z direction
#' @param train1 plot training data (T/F)
#' @param test1 plot test data (T/F)
#' @param line1 plot boundary line (T/F)
#' @param main1 main plot title
plotfun3d <- function(phi1, theta1, zlab = "Kernel value", train1 = T, test1 = F, line1 = F, main1, clab1 = 1) {


	p1 <- persp(x1[seqs], x2[seqs], z[seqs, seqs],  phi = phi1, theta = theta1,cex.main = clab1, 
		border = "grey90", xlab = "Feature 1", ylab = "Feature 2", zlab = zlab, main = main1, cex.lab = clab1)
	if(train1) {
		points(trans3D(newdat[, 1], newdat[, 2], genX[[2]], pmat = p1), cex = cex1) 
	}
	if(test1) {
		points(trans3D(newdat2[, 1], newdat2[, 2], genA[[2]], pmat = p1), 
		col = cols[2], pch = "*", cex = cex1) 
	
		points(trans3D(newdatxT[, 1], newdatxT[, 2], genXT[[2]], pmat = p1), col = cols[2], cex = cex1)
	}	
	if(line1) {
		lines(trans3D(b1[, 1], b1[, 2], genB[[2]], pmat = p1), col = cols[1], lwd = lwd1)
	}
}









########
# 3-d plots

# Set limits
lims <- c(6.5, 13.6)


# Save as png
png("viz-oneclass-3d.png", height = 600, width = 1000)
pdf("viz-oneclass-3d.pdf", height = 7, width = 11)
# 6 plots
par(mfrow = c(2, 3))


#Plot training data, 2 d
plot(newdat[, 1], newdat[, 2], xlab = "Feature 1", ylab = "Feature 2", 
     cex = cex1, xlim = lims, ylim = lims, axes = F, main = "A. Training data")

# Plot kernel, 3d
plotfun3d(phi1 = 45, theta1 = 45, main1 = "B. Estimated kernel function")

# Plot decision boundary: way to make invisible behind grid?
plotfun3d(phi1 = 45, theta1 = 45, line1 = T, 
	  main1 = "C. Estimated decision boundary")

# Plot new data: 2d
plot(newdat2[, 1], newdat2[, 2], col = cols[2], xlab = "Feature 1",
     ylab = "Feature 2", cex = cex1, xlim = lims, ylim = lims, 
     axes = F, main = "D. New data")
points(newdatxT[, 1], newdatxT[, 2], col = cols[2], cex = cex1)

# Plot decision boundary, 3d
plotfun3d(phi1 = 45, theta1 = 45, train1 = F, test1 = T, line1 = T, 
	  main1 = "E. Estimated kernel function for new data")
# Plot decision boundary, 3d
plotfun3d(phi1 = 90, theta1 = 0, train1 = F, test1 = T, line1 = T, 
	  main1 = "F. Classify new data", zlab = "")

# Plot new data: 2d with decision
#plot(newdat2[, 1], newdat2[, 2], col = cols[2], xlab = "Feature 1", 
#     ylab = "Feature 2", cex = cex1, pch = "*", xlim = lims, ylim = lims, 
#     axes = F, main = "F. Classify new data")
#points(newdatxT[, 1], newdatxT[, 2], col = cols[2], cex = cex1)
#polygon(b1, border = cols[1], lwd = lwd1)
#legend("bottomright", legend = c("Normal", "Anomalous"), pch = c(1, 8), 
#       col = cols[2])


dev.off()

# Possible addition of ggplot?
#plot.new()
#vps <- baseViewports()
#vp1 <- plotViewport(c(0, 5, 0, 0))
#print(g1, vp = vp1)

h1 <- 500
w1 <- 500

h1 <- 5
w1 <- 5


c1 <- 1.2

#pdf("viz-oneclass-3d-single1.pdf", height = h1, width = w1)

png("viz-oneclass-3d-single1.png", height = h1, width = w1)

# Save as png
#png("viz-oneclass-3d-1.png", height = h1, width = w1)
# 6 plots
#par(mfrow = c(1, 3), oma = c(0, 0, 0, 0))


#Plot training data, 2 d
#par(mar = c(20, 20, 4, 2))
plot(newdat[, 1], newdat[, 2], xlab = "Feature 1", ylab = "Feature 2",cex.lab = c1, cex.main = c1, 
     cex = cex1, xlim = lims, ylim = lims, axes = F, main = "A. Training data")


dev.off()




png("viz-oneclass-3d-single2.png", height = h1, width = w1)


#pdf("viz-oneclass-3d-single2.pdf", height = h1, width = w1)
#par(mar = c(5, 4, 4, 2))
# Plot kernel, 3d
plotfun3d(phi1 = 45, theta1 = 45, main1 = "B. Estimated kernel function")


dev.off()


png("viz-oneclass-3d-single3.png", height = h1, width = w1)
#pdf("viz-oneclass-3d-single3.pdf", height = h1, width = w1)
# Plot decision boundary: way to make invisible behind grid?
plotfun3d(phi1 = 45, theta1 = 45, line1 = T, 
	  main1 = "C. Estimated decision boundary")


dev.off()


#pdf("viz-oneclass-3d-2.pdf", height = h1, width = w1)

png("viz-oneclass-3d-single4.png", height = h1, width = w1)
#pdf("viz-oneclass-3d-single4.pdf", height = h1, width = w1)
#png("viz-oneclass-3d-2.png", height = h1, width = w1)

#par(mfrow = c(1, 3))

# Plot new data: 2d
plot(newdat2[, 1], newdat2[, 2], col = cols[2], xlab = "Feature 1",
     cex.lab = c1,cex.main = c1,
     ylab = "Feature 2", cex = cex1, xlim = lims, ylim = lims, 
     axes = F, main = "D. New data")
points(newdatxT[, 1], newdatxT[, 2], col = cols[2], cex = cex1)


dev.off()





png("viz-oneclass-3d-single5.png", height = h1, width = w1)

#pdf("viz-oneclass-3d-single5.pdf", height = h1, width = w1)

# Plot decision boundary, 3d
plotfun3d(phi1 = 45, theta1 = 45, train1 = F, test1 = T, line1 = T, 
	  main1 = "E. Estimated kernel function for new data")




dev.off()



png("viz-oneclass-3d-single6.png", height = h1, width = w1)
#pdf("viz-oneclass-3d-single6.pdf", height = h1, width = w1)
# Plot decision boundary, 3d
plotfun3d(phi1 = 90, theta1 = 0, train1 = F, test1 = T, line1 = T, 
	  main1 = "F. Classify new data", zlab = "")

# Plot new data: 2d with decision
#plot(newdat2[, 1], newdat2[, 2], col = cols[2], xlab = "Feature 1", 
#     ylab = "Feature 2", cex = cex1, pch = "*", xlim = lims, ylim = lims, 
#     axes = F, main = "F. Classify new data")
#points(newdatxT[, 1], newdatxT[, 2], col = cols[2], cex = cex1)
#polygon(b1, border = cols[1], lwd = lwd1)
#legend("bottomright", legend = c("Normal", "Anomalous"), pch = c(1, 8), 
#       col = cols[2])


dev.off()

# Possible addition of ggplot?
#plot.new()
#vps <- baseViewports()
#vp1 <- plotViewport(c(0, 5, 0, 0))
#print(g1, vp = vp1)
