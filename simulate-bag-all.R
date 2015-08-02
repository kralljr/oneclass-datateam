#load libraries 
library(e1071)

# get data
source("data-samplelabel.R")


# Create smaller data
N <- 20000
datsub <- dat1[1 : N, ]
valid <- dat1[(N + 1) : (N * 2), ]


# Get training parameters
gam1 <- seq(.01, .2, by = .02)
nu1 <- seq(.01, .2, by = .02)
cost1 <- seq(0.01, 1, by = 0.1)

# Run svm on entire subsample
set.seed(59010)
svm1 <- tune.svm(y ~ ., data = datsub, nu = nu1, gamma = gam1, cost = cost1, kernel = "radial", type = "one-classification", scale = T)
save(svm1, file = "sim-tune-svm.RData")

