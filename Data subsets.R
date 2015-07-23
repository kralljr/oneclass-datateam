# source class1 function
source("~/GitHub/oneclass-datateam/oneclass-pred.R")

# Change this directory path to where your data are stored
mylib <- "~/Documents/Work/STRAND/Data/"


# Load libraries
library(e1071)
library(dplyr)
library(caret)

# Read in data
postal1 <- read.csv(file.path(mylib, "MIT Partial Dataset.txt"))


## Run SVM using Radial Basis Function
# # Try SVM using linear kernel

postal_subset10 <- sample_n(postal1, 18000)
postal_subset25 <- sample_n(postal1, 43000)
Rprof()
y <- proc.time()
svm_model <- svm(postal1, type = "one-classification", kernal = "radial",
    scale = F)
y2 <- proc.time()
summaryRprof()
Rprof(NULL)
y2-y

pred <- predict(svm_model)
table(pred)

Rprof()
y <- proc.time()
svm_model <- svm(postal_subset10, type = "one-classification", kernal = "radial",
    scale = F)
y2 <- proc.time()
summaryRprof()
Rprof(NULL)
y2-y

pred <- predict(svm_model)
table(pred)

Rprof()
y <- proc.time()
svm_model <- svm(postal_subset25, type = "one-classification", kernal = "radial",
    scale = F)
y2 <- proc.time()
summaryRprof()
Rprof(NULL)
y2-y

pred <- predict(svm_model)
table(pred)