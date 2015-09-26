# File to test map reduce locally


# Load package
library(dplyr)
library(rmr2)
library(e1071)


# Change RMR options
rmr.options(backend = "local")

# specify dir1


source(file.path(dir1, "svm-mapr.R"))
source(file.path(dir1, "error-fun.R"))
source(file.path(dir1, "readNumberedLines.R"))

tf1 <- "~/Documents/strand/data/train.csv"
vf1 <- "~/Documents/strand/data/validation.csv"

hyperparm1 <- list()
hyperparm1$gamma <- c(0.1)
hyperparm1$nu <- c(0.1)

x <- proc.time()
Rprof()
svm1 <-   svmboot.mr(tf1, vf1, numboot = 1, sizeboot = 10000, hyperparm = hyperparm1)
Rprof(NULL)
summaryRprof()
y <- proc.time()


#15.5+ (depend on param) hours for 100 hyperparam combos for 10000 bootstrapped samples



x <- proc.time()
Rprof()
svm1 <-   svmboot.map("temp", 1, tf1, vf1, sizeboot = 10000, hyperparm = hyperparm1)
Rprof(NULL)
summaryRprof()
y <- proc.time()

