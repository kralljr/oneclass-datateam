# load libraries
library(lubridate)
library(dplyr)

# source functions
source("label-data-fn.R")



# load connectivity data
load("connectivity.Rdata")

# get filenames
lf <- list.files()
valid1 <- lf[substr(lf, 1, 4) == "trai" & substr(lf, 23, 23) == "2"]
test1 <- lf[substr(lf, 1, 5) == "testi"]
# Find training data
train1 <- lf[substr(lf, 1, 6) == "traini" & substr(lf, 23, 23) != "2"]






######################################3
# find mean and variance for scaling

binary <- c('V6','V104','V105','V106','V107','V108','V109','V120','V123','V124','V125','V139','V140','V141','V344','V345','V346','V347','V348','V350')



# for each dataset, find mean, variance
alld <- c(valid1, train1)
sums1 <- 0
ssums <- 0
n1 <- 0
for(i in 1 : length(alld)) {
  # which iteration
  print(i)
  # read in dataset
  dat <- read.csv(alld[i], header = F, stringsAsFactors = F)

  # find sums
  sums1 <- sums1 + colSums(dat[, -c(1, 2)])
  # find squares sums
  ssums <- ssums + colSums(dat[, -c(1, 2)]^2)
  n1 <- n1 + nrow(dat)
}



mns1 <- sums1 / n1
sds1 <- sqrt(1 / (n1 - 1) * (ssums - n1 * mns1^2))

# fix binary
whb <- which(colnames(dat)[-c(1, 2)] %in% binary)
mns1[whb] <- 0
sds1[whb] <- 1

#save(mns1, sds1, file = "mean-sd-all.RData")













##############################
# Create final analysis datasets


# columns to remove (no variability)
rms <- c("V57","V58","V59","V61","V63","V64","V65","V67","V69","V70","V73","V74","V75","V77","V79","V80","V81","V83","V85","V86","V89","V90","V91","V93","V95","V96","V97","V99","V101","V102")


# Concatenate all training data, rm date/ip, scale
for(i in 1 : length(train1)) {
  dat <- read.csv(train1[i], header = F, stringsAsFactors = F)

  dat <- dat[, -c(1, 2)]

  # rescale
  dat <- sweep(dat, 2, mns1, "-")
  dat[, sds1 != 0] <- sweep(dat[, sds1 != 0], 2, sds1[sds1 != 0], "/")

  # remove columns with no variability
  dat <- dat[, -which(colnames(dat) %in% rms)]

  # determine whether to append
  app1 <- ifelse(i == 1, F, T)
  print(c(i, app1))


  # save output
  filename <- "train.csv"
  write.table(dat, sep = ",",file = filename, append = app1, col.names = !app1, row.names = F)
}



# Label and save validation and test data
labelall(valid1, test1, mns1, sds1, rms)

