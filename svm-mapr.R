# File to try mapreduce from R using rmr package



####################



# map function: function of keys, values
#
# This is the function to do tuning for each bootstrapped sample
# @param k key is iteration of bootstrapping

# Issues:
# need number of lines of training (nrow1)
# need connection con
# how to input keys???
svmboot.map <- function(., k) {


  # create bootstrapped datai?
  # I think this should be done outside of map?
  set.seed(k)
  samps <- sample(1 : nrow1, sizeboot, replace = T)
  bootdat <- readNumberedLines(con, samps)

  # specify range of parameters
  gam1 <- c(seq(.01, 1, length = 5), 1.5, 2, 2.5, 3, 5)
  nu1 <- seq(.0001, .9, length = 10)

  # specify error function
  ef <- mixtype
  
  # set tuning control
  tc <- tune.control(sampling = "fix", error.fun = ef)
  
  # tune SVM on params in gam1, nu1 using dat, validx, validy
  tune1 <- tune.svm(y ~ ., data = bootdat, validation.x = validx,
    validation.y = validy, nu = nu1, gamma = gam1, cost = 1,
    kernel = "radial", type = "one-classification", scale = F,
    tunecontrol = tc)

  # find best parameters for this dataset (length 2)
  out1 <- tune1$best.parameters

  # return key/val pairs for each parameter
  keyval(c(1, 2), out1)
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
#seeds <- sample(seq(1, 100000), 1000)
#nboot= to.dfs(seeds)
#
# Function of total training and validation data, number of bootstraps and size of bootstraps
# combine = T specifies same combiner as reduce
#' @param con connection for full training dataset (no labels)
#' @param valid full validation dataset
#' @param nboot number of bootstrapped samples
#' @param sizeboot size of bootstrapped samples
svmboot.mr <- function(con, valid, numboot, sizeboot) {
  # split validation data
  validx <- dplyr::select(valid, -label)
  validy <- valid$label 
  
  fds <- from.dfs(
	 mapreduce(con, validx, validy, sizeboot,
		   input = nboot, 
		   map = svmboot.map))
                  # Reduce not necessary here 
		   #reduce = svmboot.reduce
  # get values from.dfs
  vals <- values(fds)

  # return parameters across bootstrapped samples
  return(vals)
}
