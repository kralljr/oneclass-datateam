#load libraries 
library(e1071)

# get data
source("data-samplelabel.R")


# Create smaller data
N <- 10000
datsub <- dat1[1 : N, ]
valid <- dat1[(N + 1) : (N * 2), ]


# Get training parameters
gam1 <- seq(0, 5, by = 1)
nu1 <- seq(.01, .2, by = .02)
#cost1 <- seq(0.01, 1, by = 0.1)
cost1 <- 1
nu1 <- 0.001


# Run svm on entire subsample
set.seed(59010)

tc <- tune.control(sampling = "fix", error.fun = NULL)
tune1 <- tune.svm(y ~ ., data = datsub, validation.x = validx,
    validation.y = validy, nu = nu1, gamma = gam1, cost = cost1,
    kernel = "radial", type = "one-classification", scale = T,
    tunecontrol = tc)
save(svm1, file = "sim-tune-svm.RData")

