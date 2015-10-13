#  Source error functions
source(file.path(dir2, "error-fun.R"))

# Load predictions
load(file.path(dir1, "strand-validation-predictions.Rdata"))


# Load validation data
valid <- read.csv(file.path(dir1, "validation.csv"))
truth <- valid$label


# Gammas used
gam1 <- c(seq(.01, 1, length = 5), 1.5, 2, 2.5, 3, 5)

# Get type 1 and type 2 errors for each gamma (nu = 0.0001)
errout <- matrix(nrow = ncol(validation), ncol = 2)
for(i in 1 : ncol(validation)) {
  errout[i, 1] <- type1(truth, validation[, i]) 
  errout[i, 2] <- type2(truth, validation[, i]) 
}

colnames(errout) <- c("Type 1", "Type 2")
rownames(errout) <- paste0("gamma=", gam1)

t1 <- table(validation[, 1], truth)
rowSums(t1)


