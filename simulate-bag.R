#load libraries
library(e1071)




# get bootstrap/bagging functions
source("bag-fun.R")






# get data
source("data-samplelabel.R")

# Create smaller data
seq1 <- sample(seq(1, nrow(dat1)), 20000)
datsub <- dat1[seq1[1 : 10000], ]
valid <- dat1[seq1[10001 : 20000], ]


# Get training parameters
gam1 <- seq(.01, 1, by = .1)
nu1 <- seq(.0001, 1, by = .1)
cost1 <- 1
train1 <- list(gam1 = gam1, nu1 = nu1, cost1 = cost1)

# Run bagged
set.seed(20987)
b1 <- bagsvm(datsub, valid, size = 500, nboot = 10, train1 = train1)
type <- "Simulated labels with only first two columns"
save(b1, type, file = "sim-tune-simple.RData")






################################
# Try with simulated data


set.seed(10)
x1 <- rnorm(1000)
x2 <- rnorm(1000)
y <-  (x1 < 0 & x2 < 0)
#y <- factor(y, levels = c(1, 0), labels = c("Normal", "Attack"))
dat1 <- data.frame(y, x1, x2)
datsub <- dat1[1 : 500, ]
valid <- dat1[501:1000, ]

x1n <- datsub[datsub$y == "Normal", -1]


# Try by itself (no tuning)
#x1 <- datsub[datsub$y == "Normal", -1]
x1 <- datsub[datsub$y, -1]
svm1 <- svm(x1, kernel = "radial", type = "one-classification", scale = T, nu = .0001, gamma = .1)
p1 <- predict(svm1, valid[, -1])
table(p1, valid$y)



# Try with tuning
#x1 <- datsub[datsub$y == "Normal", ]
x1 <- datsub[datsub$y, ]

tc <- tune.control(sampling = "fix", error.fun = type1a)
tune1 <- tune.svm(y ~ x1 + x2, data = dat1, validation.x = valid[, -1], 
    validation.y = valid[, 1], nu = nu1, gamma = gam1, cost = cost1, 
    kernel = "radial", type = "one-classification", scale = T,
    tunecontrol = tc)

svm1 <- tune1$best.model
p1 <- predict(svm1, valid[, -1])
table(p1, valid$y)
