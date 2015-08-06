#load files
load("sim-tune.RData")
load("sim-tune-svm.RData")


#source functions
source("simulate-bag.R")


#ensemble class
ens1 <- b1$ens

# predict on total
tot1 <- predict(svm1$best.model, valid)
tot1 <- 1 - 1 * tot1

# nu small = no outliers allowed
# gamma small = large variance on kernel
# cost small = no penalty term for lagrange

geterrs(tot1, valid)

geterrs(ens1, valid)


table(valid$y, tot1)
table(valid$y, ens1)
table(tot1, ens1)
