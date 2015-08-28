#load libraries
library(e1071)



# get bootstrap/bagging functions
source("bag-fun.R")


# get data
source("data-samplelabel.R")

# Create smaller data

nsize <- (nrow(dat1) - 1) / 2 - 1
seq1 <- sample(seq(1, nrow(dat1)), nsize * 2)
datsub <- dat1[seq1[1 : nsize], ]
valid <- dat1[seq1[(nsize + 1) : (nsize * 2)], ]


# Get training parameters
gam1 <- seq(.01, 1, by = .1)
nu1 <- seq(.0001, 1, by = .1)
cost1 <- 1
train1 <- list(gam1 = gam1, nu1 = nu1, cost1 = cost1)



# Time 1 tune on 1000 samples

x <- proc.time()
i1 <- innerbag(datsub, valid, 1000, nrow(datsub), gam1 = gam1, nu1 = nu1, cost1 = 1, ef = NULL)
y <- proc.time()


y - x


#23.7 seconds



# Single process
x <- proc.time()

N <- nrow(datsub)
size <- 50000
sample1 <- sample(seq(1, N), size = size, replace = T)
dats <- datsub[sample1, ]


# Tune
dat1 <- dats[dats$y, ]
validx <- valid[, -1]
validy <- valid[, 1]

svm_model <- svm(dat1[, -1], kernel = "radial", scale = T, type = "one-classification", gamma = 0.01, nu = 0.0001)
p1 <- predict(svm_model, validx)
t1 <- table(p1, validy)
sum(t1 - diag(diag(t1))) / nrow(validx)


y <- proc.time()

y - x








# All data 
x <- proc.time()
svm_model <- svm(datsub[, -1], kernel = "radial", scale = T, type = "one-classification", gamma = 0.01, nu = 0.0001)


y <- proc.time()

y - x



# Tune for all


x <- proc.time()

# Tune
# start <- proc.time()
dat1 <- datsub[datsub$y, ]
validx <- valid[, -1]
validy <- valid[, 1]

ef <- NULL
tc <- tune.control(sampling = "fix", error.fun = ef)
tune1 <- tune.svm(y ~ ., data = dat1, validation.x = validx,
  validation.y = validy, nu = nu1, gamma = gam1, cost = cost1,
 kernel = "radial", type = "one-classification", scale = T,
 tunecontrol = tc)

y <- proc.time()

y - x
