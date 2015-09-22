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






######
# Check labelling function
labelall(valid1[1], test1[1])

dat <- read.csv("validation.csv")
dat <- read.csv("test.csv")


length(which(dat$label != 0)) / length(dat$label)







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

min1 <- min1 - 2
max1 <- max1 + 2 

connect1 <- dplyr::filter(connect1, datetime <= max1 & datetime >= min1)
# Restrict to connections with victim IP
connect1 <- dplyr::filter(connect1, substr(src, 1, 11) == vip | substr(dst, 1, 11) == vip) 


data.frame(label1, dat2[, c(1, 2)])
connect1











##### Test for labelling

# Use validation, day 2
j <- 3
dat <- read.csv(valid1[j], header = F, stringsAsFactors = F)
# Find good window

dat[, 1] <- ymd_hms(dat[, 1])
colnames(dat)[1] <- "date"

# Run labeller
gl1 <- getlabel(dat, valid1[j])
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
connect1 <- get(sapply(strsplit(gsub("-", "_", valid1[j]), "\\."),
  function(x) x[1]))
connect1$src <- as.character(connect1$src)
connect1$dst <- as.character(connect1$dst)
m1 <- m1 - 5
m2 <- m2 + 5

connect1 <- dplyr::filter(connect1, datetime <= m2 & datetime >= m1)
# Restrict to connections with victim IP
connect2 <- dplyr::filter(connect1, substr(src, 1, 11) == vip | substr(dst, 1, 11) == vip) 

# Should either be 0 or 2
data.frame(labs2, 1  * (substr(dat2[, 2], 1, 11) == vip)) %>% rowSums() %>% table()


# These should not match
connect2
dat2[which(labs2 == 0), ]


data.frame(labs2, dat2[, 2])

dat2[labs2 == 2, c(1, 2)]






##### Test for labelling

# 03 11 20:47:15



# Use validation, day 2
j <- 4
dat <- read.csv(valid1[j], header = F, stringsAsFactors = F)
# Find good window

dat[, 1] <- ymd_hms(dat[, 1])
colnames(dat)[1] <- "date"

# Run labeller
gl1 <- getlabel(dat, valid1[j])
labs <- gl1$label

m1 <- attack[24, 1] - 30
m2 <- attack[24, 1] + 30


labs2 <- labs[which(dat$date < m2 & dat$date > m1)]
dat2 <- dat[which(dat$date < m2 & dat$date > m1), c(1, 2)]
attack[24, c(1, 2)]


# Check against truth
vip <- as.character(attack[24, 2])

# get connection data
connect1 <- get(sapply(strsplit(gsub("-", "_", valid1[j]), "\\."),
  function(x) x[1]))
connect1$src <- as.character(connect1$src)
connect1$dst <- as.character(connect1$dst)
m1 <- m1 - 2
m2 <- m2 + 2

connect1 <- dplyr::filter(connect1, datetime <= m2 & datetime >= m1)
# Restrict to connections with victim IP
connect2 <- dplyr::filter(connect1, substr(src, 1, 15) == vip | substr(dst, 1, 15) == vip) 

# Should either be 0 or 2
df1 <- data.frame(labs2, 1  * (substr(dat2[, 2], 1, 15) == vip)) 
df1 %>% rowSums() %>% table()


# These should not match
ids <- unique(unlist(connect2[, -1]))
dat2[which(labs2 == 0 & dat2[, 2] %in% ids), ]


data.frame(labs2, dat2[, 2])

dat2[labs2 == 2, c(1, 2)]







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
min1 <- t1 - 4
max1 <- t1 + 4

# Connection data in window
wh2 <- connect1[which(connect1[, 1] <= max1 & connect1[, 1] >= min1),]


# IPs not in connection data
unfeat <- unique(datsub[, 2])
unconn <- unique(unlist(wh2[, c(2, 3)]))
unfeat[!(unfeat %in% unconn)]
