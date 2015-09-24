#load libraries
library(e1071)
library(dplyr)


# Source error function
source("bag-fun.R")
source("error-fun.R")

# load means
load("mean-sd-all.RData")


## Check training time

# get data
lf <- list.files()
lf <- lf[substr(lf, 1, 6) == "traini"]

dat1 <- read.csv(lf[1], header = F, stringsAsFactors = F)
dat2 <- dat1[, -c(1, 2)]


# Get training parameters
gam1 <- 3
nu1 <- .9
cost1 <- 1



# columns to remove (no variability)
rms <- c("V57","V58","V59","V61","V63","V64","V65","V67","V69","V70","V73","V74","V75","V77","V79","V80","V81","V83","V85","V86","V89","V90","V91","V93","V95","V96","V97","V99","V101","V102")




ns <- c(100, 1000, 10000)
ns <- 10000
for(i in 1 : length(ns)) {

  seq1 <- sample(seq(1, nrow(dat2)), ns[i] )
  datsub <- dat2[seq1, ]
  
  # mean 0
  datsub <- sweep(datsub, 2, mns1, "-")
  sdT <- sds1 != 0
  datsub[, sdT] <- sweep(datsub[, sdT], 2, sds1[sdT], "/")

  datsub <- datsub[, -which(colnames(datsub) %in% rms)]
  
  
  x <- proc.time()
  
  svm_model <- svm(datsub, kernel = "radial", scale = F, nu = nu1, gam = gam1, type = "one-classification")
  
  y <- proc.time()
  print(y - x)

}

# 3 minutes for 10000
#14 minutes for 25000



######
# fix training data


# Time 1 tune on N samples
ef <- mixtype
tc <- tune.control(sampling = "fix", error.fun = ef)

datsub2 <- mutate(datsub, label = T)

n1 <- ncol(datsub2) - 1
colnames(datsub2)[-which(colnames(datsub2) == "label")] <- paste0("col", seq(1, n1))





######
# fix validation data
## Check validation size

valid1 <- read.csv("validation.csv", stringsAsFactors = F, colClasses = c(rep("numeric", 318), "logical"))



validx <- dplyr::select(valid1, -label)
validy <- valid1$label
colnames(validx) <- paste0("col", seq(1, n1))

# test with subset of validation data
n2 <- 10000 
validx <- validx[1 : n2, ]
validy <- validy[1 : n2]










# add more than 1 parameter
nu1 <- c(0.5, 0.9)
gam1 <- c(0.1, 3)

x <- proc.time()
tune1 <- tune.svm(label ~ ., data = datsub2, validation.x = validx,
                  validation.y = validy, nu = nu1, gamma = gam1, cost = cost1,
                  kernel = "radial", type = "one-classification", scale = F,
                  tunecontrol = tc)
y <- proc.time()
print(y - x)




# Try with svm, apply function
colnames(datsub) <- paste0("col", seq(1, ncol(datsub)))

x <- proc.time()

svm_model <- svm(datsub, kernel = "radial", scale = F, nu = nu1[2], gam = gam1[2], type = "one-classification")

p1 <- predict(svm_model, validx)
mt <- mixtype(validy, p1)


y <- proc.time()
print(y - x)





