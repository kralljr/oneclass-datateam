#load libraries
library(e1071)
library(dplyr)




# get data
dat1 <- read.csv("test.csv", stringsAsFactors = F)
valid1 <- read.csv("validation.csv", stringsAsFactors = F)

validx <- dplyr::select(valid1, -label)
validy <- valid1$label


# Get training parameters
gam1 <- seq(.01, 1, by = .1)
nu1 <- seq(.0001, 1, by = .1)

gam1 <- 0.01
nu1 <- 0.0001
cost1 <- 1
train1 <- list(gam1 = gam1, nu1 = nu1, cost1 = cost1)



# Time 1 tune on N samples
ef <- NULL
tc <- tune.control(sampling = "fix", error.fun = ef)


ns <- c(100, 1000, 10000)
for(i in 1 : 3) {

  seq1 <- sample(seq(1, nrow(dat1)), ns[i] )
  datsub <- dat1[seq1, ]
  datsub <- data.frame(seq(T, nrow(datsub)), datsub)
  colnames(datsub)[1] <- "label"
  
  
  x <- proc.time()
  tune1 <- tune.svm(label ~ ., data = dat1, validation.x = validx,
                    validation.y = validy, nu = nu1, gamma = gam1, cost = cost1,
                    kernel = "radial", type = "one-classification", scale = T,
                    tunecontrol = tc)
  y <- proc.time()
  print(y - x)

}
