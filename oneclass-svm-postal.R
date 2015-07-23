# source class1 function
source("oneclass-pred.R")

# Change this directory path to where your data are stored
mylib <- "~/Dropbox/oneclass/data"


# Load libraries
library(e1071)
library(dplyr)

# Read in data
postal1 <- read.csv(file.path(mylib, "train.1.1"))
postal2 <- read.csv(file.path(mylib, "train.2.2"))


# Plot two sample pixels to use for SVM exploration
j <- 182
k <- 198
plot(postal1[, j], postal1[, k], xlab = paste("Pixel", j), 
     ylab = paste("Pixel", k), xlim = c(-1, 1.5), ylim = c(-1, 1.5))
points(postal2[, j], postal2[, k], col = "red")
legend("bottomright", legend = c("1's", "2's"), col = c("black", "red"), 
     pch = 16)


# Try SVM using linear kernel
svm_model <- svm(postal1[, c(j, k)], kernel = "linear", 
    scale = F)


# For each observation, create prediction
pred1 <- vector()
for(l in 1 : nrow(postal1)) {
    newvec <- as.numeric(postal1[l, c(j, k)])
    # Find Kernel: u'v
    kern <- rowSums(sweep(svm_model$SV, 2, newvec, "*"))
    # Find pred
    val1 <- sum(svm_model$coefs * kern) - svm_model$rho
    pred1[l] <- sign(val1)
}

# Our computed predictions match prediction from svm function
table(pred1)
pred2 <- predict(svm_model)
table(pred2)

# Check against using my function
postaljk <- as.matrix(postal1[, c(j, k)])
testfn <- class1(postaljk, postaljk, kernel= "linear", type = NULL)
table(testfn)


# Plot points by prediction
# Specify colors based on predictions
cols <- ifelse(pred2, "black", "red")
# Plot points by prediction
plot(postal1[, j], postal1[, k], col = cols)
legend("topleft", legend = c("Not anomalous", "Anomalous"),
    pch = 16, col = c("black", "red"))


## Run SVM using Radial Basis Function
svm_model <- svm(postal1[, c(j, k)], type = "one-classification", 
    scale = F)

# Predict from model
pred <- predict(svm_model)
table(pred)

# Predict from my function
testfn <- class1(postaljk,  dat = postaljk, kernel = "radial")
table(testfn)


# Compute boundary

# Sample grid points near boundary
N <- 100
x1 <- seq(-1, -0.7, length = N)
x2 <- x1
xs <- cbind(rep(x1, N), rep(x2, each = N))

# Classify grid points
c1 <- class1(xs, "radial", svm1 = svm_model)

# Color based on anomaly
cols <- ifelse(c1 == 1, "blue", "black")
plot(xs[, 1], xs[, 2], col = cols, xlab = paste("Pixel", j), 
     ylab = paste("Pixel", k), xlim = c(-1, -.5), ylim = c(-1, -.5))
legend("topright", legend = c("normal", "anomalous"), col = c("blue", "black"), 
     pch = 16)


# Plot convex hull
true <- xs[c1 == 1, ]
b1 <- true[chull(true), ]

plot(1, 1, type = "n", xlab = paste("Pixel", j), 
     ylab = paste("Pixel", k), xlim = c(-1.2, 1), ylim = c(-1.2, 1))
polygon(b1, lwd = 2)
text(-1, -1.1, labels = "Anomalous")
text(-.95, .9, labels = "Not anomalous")

cols <- ifelse(predict(svm_model), "red", "blue")
points(postaljk[, 1], postaljk[, 2], col = cols)

