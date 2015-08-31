# File to try mapreduce from R using rmr package



####################
# map function: function of keys, values
#
# This is the function to do tuning for each bootstrapped sample
# @param k key is iteration of bootstrapping
svmboot.map <- function(., k) {


  # create bootstrapped datai?
  # I think this should be done outside of map?
  set.seed(k)
  samps <- sample(1 : nrow(dat), sizeboot, replace = T)
  bootdat <- dat[samps, ]

  # specify range of parameters
  gam1 <- seq(.01, 1, by = .1)
  nu1 <- seq(.0001, 1, by = .1)

  # specify error function (for now, null)?
  ef <- NULL
  
  # set tuning control
  tc <- tune.control(sampling = "fix", error.fun = ef)
  
  # tune SVM on params in gam1, nu1 using dat, validx, validy
  tune1 <- tune.svm(y ~ ., data = bootdat, validation.x = validx,
    validation.y = validy, nu = nu1, gamma = gam1, cost = 1,
    kernel = "radial", type = "one-classification", scale = T,
    tunecontrol = tc)

  # find best parameters for this dataset
  out1 <- tune1$best.parameters

  # return something
  keyval(k, out1)
}








####################
# reduce function
#
# I don't think we need this, since we want to just spit out all best.parameters
# @param k key is iteration of bootstrapping
# @param vv value is best.parameters output of tune.svm
svmboot.reduce <- function(k, vv) {
    keyval(k, vv)
}



######################
# Full mapreduce function
#
# Specify seeds for jobs/ bootstrapped samples
seeds <- sample(seq(1, 100000), 1000)
nboot= to.dfs(seeds)
#
# Function of total training and validation data, number of bootstraps and size of bootstraps
svmboot.mr <- function(dat, valid, numboot, sizeboot, combine) {
  # split validation data
  validx <- valid[, -1]
  validy <- valid[, 1]
  fds <- from.dfs(
	 mapreduce(dat, validx, validy, sizeboot,
		   input = nboot, 
		   map = svmboot.map, 
		   reduce = svmboot.reduce))
  # get values from.dfs
  vals <- values(fds)

  # return parameters across bootstrapped samples
  return(vals)
}
