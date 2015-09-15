# File to add labels to validation and test data

# load libraries
library(lubridate)
library(dplyr)

# source functions
source("label-data-fn.R")


# Set working directory to data directory

# load connectivity data
load("connectivity.Rdata")

# get filenames
lf <- list.files()
valid1 <- lf[substr(lf, 1, 4) == "trai" & substr(lf, 23, 23) == "2"]
test1 <- lf[substr(lf, 1, 5) == "testi"]



# Try for validation data
gl1 <- getlabel(valid1[1])

# check against truth
dat <- read.csv(valid1[1], header = F, stringsAsFactors = F)
colnames(dat)[1 : 2] <- c("date", "ip")
attack <- read.csv("validation-labels.csv", stringsAsFactors = F)


head(dat[gl1 == "attack", ])

