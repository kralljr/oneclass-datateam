# load libraries
library(e1071)

# load test subdata
setwd(dir1)

#load code
source(file.path(dir2, "readNumberedLines.R"))
source(file.path(dir2, "error-fun.R"))



set.seed(10)
nrow1 <- 1249321
N <- 10000
samps <- sample(seq(2, nrow1), 10000)
dat <- read.csv(textConnection(readNumberedLines("train.csv", samps)), colClasses = "numeric", header = F)



# read validation data
#valid1 <- read.csv("validation-small.csv")

#validx <- valid1[, -which(colnames(valid1) == "label")]
#validy <- valid1$label


# read some test data
set.seed(10)
nrow1 <- 1249321
N <- 100000
test <- read.csv(textConnection(readNumberedLines("test.csv", samps)), colClasses = c(rep("numeric", 318), "logical"), header = F)

testx <- test[, -319]
testy <- test[, 319]


gam1 <- 3
nu1 <- .9

# fit SVM on params in gam1, nu1 using dat, validx, validy
svm_model <- svm(dat, kernel = "radial", scale = F, type = "one-classification", gamma = gam1, nu = nu1)

pred1 <- predict(svm_model, testx)

type1(testy, pred1)
type2(testy, pred1)
