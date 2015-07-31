# load libraries
library(e1071)


# get data
source("data-samplelabel.R")

bagsvm <- function(dat, valid, size, nboot, train1 = NULL) {
	# Number of rows
	N <- nrow(dat)
	
	# Save output
	classmat <- matrix(nrow = nrow(valid), ncol = nboot)
	params <- matrix(nrow = 3, ncol = nboot)
	
	# Get training values
	if(is.null(train1)) {
		gam1 <- seq(.1, .2, by = .1)
		nu1 <- seq(.1, .2, by = .1)
		cost1 <- seq(100, 1000, by = 100)
	}else{
		gam1 <- train1$gam1
		nu1 <- train1$nu1
		cost1 <- train1$cost1
		
	}
	
	# For each bootstrapped sample
	for(i in 1 : nboot) {
		print(i)
		# Do one svm
		ib1 <- innerbag(dat, valid, size, N, gam1, nu1, cost1)
		# Save output
		classmat[, i] <- ib1$class1
		params[, i] <- ib1$param
	}
	
	# Get summary of classmat
	ens <- round(apply(classmat, 1, mean))
	
	# Return output
	out <- list(ens = ens, class = classmat, params = params)
	return(out)
}


innerbag <- function(dat, valid, size, N, gam1, nu1, cost1) {
	
	# subsample
	sample1 <- sample(seq(1, N), size = size, replace = T)
	dats <- dat[sample1, ]

	
	# Tune
	# start <- proc.time()
	tune1 <- tune.svm(y ~ ., data = dats, nu = nu1, gamma = gam1, cost = cost1, kernel = "radial", type = "one-classification", scale = T)
	#
	# stop <- proc.time()
	# (stop - start)[3]

	# Predict on validation
	class1 <- predict(tune1$best.model, valid)
	
	# Get params
	param <- as.matrix(tune1$best.parameters)
	
	# Get output
	out <- list(param = param, class1 = class1)
	return(out)
	
}



geterrs <- function(class1, valid) {
	true_class <- valid$y
	
	# FP: Given normal, call attack
	type1 <- class1[true_class == "Normal"] 
	type1 <- length(which(fp == 0)) / length(fp)
	
	# FN: Given attack, call normal
	type2 <- class1[true_class == "Attack"] 
	type2 <- length(which(fp == 1)) / length(fp)
	
	# Get output
	out <- c(type1, type2)
	names(out) <- c("type1", "type2")
	
	return(out)
}


# Create smaller data
datsub <- dat1[1 : 20000, ]

valid <- dat1[20001 : 40000, ]



# Get training parameters
gam1 <- seq(.01, .2, by = .02)
nu1 <- seq(.01, .2, by = .02)
cost1 <- seq(0.01, 1, by = 0.1)
train1 <- list(gam1 = gam1, nu1 = nu1, cost1 = cost1)

# Run bagged
set.seed(20987)
#b1 <- bagsvm(datsub, valid, size = 500, nboot = 100, train1 = train1)
#save(b1, file = "sim-tune.RData")

# Run svm on entire subsample
set.seed(59010)
svm1 <- tune.svm(y ~ ., data = datsub, nu = nu1, gamma = gam1, cost = cost1, kernel = "radial", type = "one-classification", scale = T)
class1 <- predict(svm1$best.model, valid)
save(class1, file = "sim-tune-svm.RData")



# Compare classification on ensemble vs. full


