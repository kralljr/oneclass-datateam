#load libraries
library(e1071)
library(dplyr)




## Check training time

# get data
lf <- list.files()
lf <- lf[substr(lf, 1, 6) == "traini"]

dat1 <- read.csv(lf[1], stringsAsFactors = F)
dat2 <- dat1[, -c(1, 2)]


# Get training parameters
gam1 <- 3
nu1 <- .9
cost1 <- 1


ns <- c(100, 1000, 10000)
ns <- 10000
for(i in 1 : length(ns)) {

  seq1 <- sample(seq(1, nrow(dat2)), ns[i] )
  datsub <- dat2[seq1, ]
  
  # mean 0
  mn1 <- colMeans(datsub)
  datsub <- sweep(datsub, 2, mn1, "-")

  
  sd1 <- apply(datsub, 2, sd)
  wh1 <- which(sd1 == 0) 
  if(length(wh1) > 0) {
    datsub[, -wh1] <- sweep(datsub[, -wh1], 2, sd1[-wh1], "/")

  }else{

  }

  
  
  x <- proc.time()
  
  svm_model <- svm(datsub, kernel = "radial", scale = F, nu = nu1, gam = gam1, type = "one-classification")
  
  y <- proc.time()
  print(y - x)

}

# 3 minutes for 10000
#14 minutes for 25000




## Check validation size

valid1 <- read.csv("validation.csv", stringsAsFactors = F)



valid2 <- valid1[, -c(1, 2)]

validx <- dplyr::select(valid2, -label)

mn1 <- apply(validx, 2, mean)
validx <- sweep(validx, 2, mn1, "-")
sd1 <- apply(validx, 2, sd)
sd1[sd1 == 0] <- 1
validx <- sweep(validx, 2, sd1, "/")


validy <- valid2$label





# Time 1 tune on N samples
ef <- NULL
tc <- tune.control(sampling = "fix", error.fun = ef)

datsub2 <- mutate(datsub, label = T)

n1 <- ncol(datsub2) - 1
colnames(datsub2)[-which(colnames(datsub2) == "label")] <- paste0("V", seq(1, n1))
colnames(validx) <- paste0("V", seq(1, n1))


x <- proc.time()
tune1 <- tune.svm(label ~ ., data = datsub2, validation.x = validx,
                  validation.y = validy, nu = nu1, gamma = gam1, cost = cost1,
                  kernel = "radial", type = "one-classification", scale = F,
                  tunecontrol = tc)
y <- proc.time()
print(y - x)
