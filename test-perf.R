# load libraries
library(e1071)

# load test subdata
setwd(dir1)

#load code
source(file.path(dir2, "readNumberedLines.R"))
source(file.path(dir2, "error-fun.R"))



set.seed(10)
nrow1 <- 1249321
samps <- sample(seq(2, nrow1), 10000)
dat <- read.csv(textConnection(readNumberedLines("train.csv", samps)), colClasses = "numeric", header = F)



# read validation data
valid1 <- read.csv("validation-small.csv")

validx <- valid1[, -which(colnames(valid1) == "label")]
validy <- valid1$label





gam1 <- .6
nu1 <- 0.1

# fit SVM on params in gam1, nu1 using dat, validx, validy
svm_model <- svm(dat, kernel = "radial", scale = F, type = "one-classification", gamma = gam1, nu = nu1)

pred1 <- predict(svm_model, validx)

type1(validy, pred1)
type2(validy, pred1)
