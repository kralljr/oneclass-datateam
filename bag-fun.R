# Functions to perform bagging of SVM


# Function to perform bagging of SVM
#' @param dat dataset to be bootstrapped
#' @param valid validation dataset with labels
#' @param size size of bootstrapped sample
#' @param nboot number of bootstrapped samples
#' @param ef error function
#' @param train1 hyperparameter range for training
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





# Function to run SVM on subsample
#' @param dat dataset to sample
#' @param valid validation data
#' @param size size of bootstrapped sample
#' @param N number of rows of dataset
#' @param gam1 gamma parameter range for training
#' @param nu1 nu parameter range for training
#' @param cost1 cost parameter range for training
#' @param ef error function
innerbag <- function(dat, valid, size, N, gam1, nu1, cost1, ef) {

  # subsample
  sample1 <- sample(seq(1, N), size = size, replace = T)
  dats <- dat[sample1, ]


  # Tune
  # start <- proc.time()
  dat1 <- dats[dats$label, ]
  validx <- dplyr::select(valid, -label)
  validy <- valid$label


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







