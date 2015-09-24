# oneclass-datateam
One-class SVM code files for the data team

The USPS zipcode dataset can be found here: http://statweb.stanford.edu/~tibs/ElemStatLearn/data.html
  1. Download the digit data for 1's and 2's (train.1, train.2)
  2. Use `read.csv` in R to read in the data: e.g. `postal1 <- read.csv("train.1.1")`


## Files for external use
  1. bag-svm.R  Functions to perform bagged training of one-class SVMs and possible error functions.
  2. label-data-fn.R Functions to label data
  3. svm-map.R Functions for mapreduct for bootstrap.  Need to decide number of bootstrap samples, size of bootstrap samples, error function
  4. error-fun.R Error functions to use in SVM

## Files for data team use and exploration
  1. compare-bag.R file to check error function and SVM results
  2. data-samplelabel.R file to produce sample labels on unlabelled "fv.uniq.txt" data
  3. "Data subsets.R" file to check optimal hyperparameters on subsets of data
  4. label-data.R Test labeling using label-data-fn.R
  5. oneclass-3d.R File to create 3-d plots of one class classifier
  6. oneclass-bagging-v1.R Sean's file to show sample SVM bagging application
  7. oneclass-bagging-v2.R Sean's second file to show sample SVM bagging.
  8. oneclass-fig.R file to create example figures of one class SVM example.
  9. oneclass-sim.Rmd file to run simulation to show how SVM performance changes with changes in kernel and hyperparameters.
  10. oneclass-svm-postal.R file to test one class SVM on postal code data
  11. simulate-bag-all.R file to demonstrate tuning on subsample of example data
  12. simulate-bag.R file to demonstrate tuning on subsample of example data using simple and more complex cases
  13. time-svm-cluster.R Function to time SVMs prior to running on cluster
  14. create-training.R File to create training, validation, and test datasets for use
  15. time-full-valid.R File to time SVM training and testing
