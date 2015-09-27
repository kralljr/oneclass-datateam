
# File to create smaller validation data


# load libraries
library(lubridate)
library(dplyr)



# source functions
source("~/Documents/repos/oneclass-datateam/label-data-fn.R")



# load connectivity data
load("connectivity.Rdata")
# load mean/sd
load("mean-sd-all.RData")




lf <- list.files(dir1)
valid1 <- lf[substr(lf, 1, 4) == "trai" & substr(lf, 23, 23) == "2"]

# columns to remove (no variability)
rms <- c("V57","V58","V59","V61","V63","V64","V65","V67","V69","V70","V73","V74","V75","V77","V79","V80","V81","V83","V85","V86","V89","V90","V91","V93","V95","V96","V97","V99","V101","V102")



for(i in 1 : length(valid1)) {

  dat <- read.csv(valid1[i], header = F, stringsAsFactors = F)

  # remove date and ip
  datd <- dat[, c(1, 2)]
  dat <- dat[, -c(1, 2)]

  # rescale
  dat <- sweep(dat, 2, mns1, "-")
  dat[, sds1 != 0] <- sweep(dat[, sds1 != 0], 2, sds1[sds1 != 0], "/")


  if(!is.null(rms)) {
    dat <- dat[, -which(colnames(dat) %in% rms)]
  }

  dat <- data.frame(datd, dat)
  dat1 <- getlabel(dat, valid1[i])

  app1 <- ifelse(i == 1, F, T)
  print(c(i, app1))



  wh1 <- which(dat1$label == F)
  wh2 <- 0
  for(j in wh1) {
     min1 <- dat1[j, 1] - 60 * 30 
     max1 <- dat1[j, 1] + 60 * 30
     wh2 <- c(wh2, which(dat1[, 1] >= min1 & dat1[, 1] <= max1))
  }
  wh2 <- unique(wh2[-1])
  
  
  dat1 <- dat1[wh2, -c(1, 2)]
  
  write.table(dat1, sep = ",", file = "validation-small.csv", append = app1, col.names = !app1, row.names = F)

}



#x <- read.csv("validation-small.csv")
