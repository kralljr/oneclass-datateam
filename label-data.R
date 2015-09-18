# File to add labels to validation and test data

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



############ Only function needed to label and create validation and test datasets
# Label all and output datasets
labelall(valid1, test1)





##### Test for labelling

# Use validation, day 2
j <- 2 
dat <- read.csv(valid1[j], header = F, stringsAsFactors = F)
# Find good window
rows <- 28280 : 28290
dat2 <- dat[rows, ]
dat2[, 1] <- ymd_hms(dat2[, 1])

# Find window to check
min1 <- min(dat2[, 1]) - 10
max1 <- max(dat2[, 1]) + 10


# Run labeller
gl1 <- getlabel(dat2, valid1[j])
label1 <- data.frame(rows, gl1$label)


# Check against truth
# get attack data
attack <- read.csv("validation-labels.csv", stringsAsFactors = F)
attack[, 1] <- ymd_hms(attack[, 1])

attack2 <- dplyr::filter(attack, datetime <= max1 & datetime >= min1)
vip <- as.character(attack2[2])

# get connection data
connect1 <- get(sapply(strsplit(gsub("-", "_", valid1[2]), "\\."),
  function(x) x[1]))
connect1$src <- as.character(connect1$src)
connect1$dst <- as.character(connect1$dst)
connect1 <- dplyr::filter(connect1, datetime <= max1 & datetime >= min1)
# Restrict to connections with victim IP
connect1 <- dplyr::filter(connect1, substr(src, 1, 11) == vip | substr(dst, 1, 11) == vip) 


dat2[, c(1, 2)]
connect1
label1

#28285 should be 1
#28288 should be 2








##### Test for labelling

# Use validation, day 2
j <- 3
dat <- read.csv(valid1[j], header = F, stringsAsFactors = F)
# Find good window

dat[, 1] <- ymd_hms(dat[, 1])
colnames(dat)[1] <- "date"

# Run labeller
gl1 <- getlabel(dat2, valid1[j])
labs <- gl1$label

m1 <- attack[17, 1] - 30
m2 <- attack[17, 1] + 30


labs2 <- labs[which(dat$date < m2 & dat$date > m1)]
dat2 <- dat[which(dat$date < m2 & dat$date > m1), c(1, 2)]
attack[17, c(1, 2)]


# Check against truth
# get attack data
attack <- read.csv("validation-labels.csv", stringsAsFactors = F)
attack[, 1] <- ymd_hms(attack[, 1])

vip <- as.character(attack[17, 2])

# get connection data
connect1 <- get(sapply(strsplit(gsub("-", "_", valid1[2]), "\\."),
  function(x) x[1]))
connect1$src <- as.character(connect1$src)
connect1$dst <- as.character(connect1$dst)
connect1 <- dplyr::filter(connect1, datetime <= max1 & datetime >= min1)
# Restrict to connections with victim IP
connect1 <- dplyr::filter(connect1, substr(src, 1, 11) == vip | substr(dst, 1, 11) == vip) 


data.frame(labs2, 1  * (substr(dat2[, 2], 1, 11) == vip)) %>% rowSums() %>% table()

unique(dat2[labs2 == 0, 2])
unique(unlist(connect1[, c(2, 3)]))



####################### 
# Section to highlight issue IPs not in connectivity data

# Time on day 2 validation data
t1 <- ymd_hms("1999-03-09T18:05:10Z")

# Window for feature data
min1 <- t1 - 2
max1 <- t1 + 2


# Read in validation data for day 2
dat <- read.csv(valid1[2], header = F, stringsAsFactors = F)
dat[, 1] <- ymd_hms(dat[, 1])

# Find feature data in window
wh1 <- which(dat[, 1] <= max1 & dat[, 1] >= min1)
datsub <- dat[wh1, ]

# get connection data
connect1 <- get(sapply(strsplit(gsub("-", "_", valid1[2]), "\\."),
  function(x) x[1]))

# Window for connection data
min1 <- t1 - 10
max1 <- t1 + 10

# Connection data in window
wh2 <- connect1[which(connect1[, 1] <= max1 & connect1[, 1] >= min1),]


# IPs not in connection data
unfeat <- unique(datsub[, 2])
unconn <- unique(unlist(wh2[, c(2, 3)]))
unfeat[!(unfeat %in% unconn)]
