bagsvm <- function(dat, valid, size, nboot, ef = NULL, train1 = NULL) {
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
		ib1 <- innerbag(dat, valid, size, N, gam1, nu1, cost1, ef)
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






innerbag <- function(dat, valid, size, N, gam1, nu1, cost1, ef) {

	# subsample
	sample1 <- sample(seq(1, N), size = size, replace = T)
	dats <- dat[sample1, ]


	# Tune
	# start <- proc.time()
	dat1 <- dats[dats$y, ]
	validx <- valid[, -1]
	validy <- valid[, 1]


	tc <- tune.control(sampling = "fix", error.fun = ef)
	tune1 <- tune.svm(y ~ ., data = dat1, validation.x = validx, 
	    validation.y = validy, nu = nu1, gamma = gam1, cost = cost1, 
	    kernel = "radial", type = "one-classification", scale = T,
	    tunecontrol = tc)

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
	#type1 <- class1[true_class == "Normal"] 
	type1 <- class1[true_class]
	type1 <- length(which(type1 == 0)) / length(type1)

	# FN: Given attack, call normal
	#type2 <- class1[true_class == "Attack"] 
	type2 <- class1[!true_class]
	type2 <- length(which(type2 == 1)) / length(type2)

	# Get output
	out <- c(type1, type2)
	names(out) <- c("type1", "type2")

	return(out)
}






type1 <- function(true, class1) {
	# which normal
	class2 <- class1[true]
	# which attack | normal
	x1 <- class2[!class2]
	t1 <- length(x1) / length(class2)
	t1
}


type2 <- function(true, class1) {
	# which attack
	class2 <- class1[!true]
	# which normal | attack
	x1 <- class2[class2]
	t1 <- length(x1) / length(class2)
	t1
}

mixtype <- function(true, class1, w) {
	type1U <- type1(true, class1)
       type2U <- type2(true, class1)
	out <- (1 - w) * type1U + w * type2U
	return(out)       

}
