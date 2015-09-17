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



# time for validation data
x <- proc.time()
gl1 <- getlabel(valid1[3])
y <- proc.time()
y- x



# check against truth
dat <- read.csv(valid1[1], header = F, stringsAsFactors = F)
colnames(dat)[1 : 2] <- c("date", "ip")
attack <- read.csv("validation-labels.csv", stringsAsFactors = F)


head(dat[gl1 == "attack", ])



# Run all
labelall(valid1, test1)
