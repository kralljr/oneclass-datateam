# oneclass-datateam
One-class SVM code files for the data team

The USPS zipcode dataset can be found here: http://statweb.stanford.edu/~tibs/ElemStatLearn/data.html
  1. Download the digit data for 1's and 2's (train.1, train.2)
  2. Use `read.csv` in R to read in the data: e.g. `postal1 <- read.csv("train.1.1")`


There are currently two .R files
  1. oneclass-pred.R that contains a function I wrote for obtaining predictions from our SVM model.  This was mostly to make sure I understood what predict(svm) does, and you may not need this function
  2. oneclass-svm-postal.R is an exploration of the postal code data and sample SVMs.  At the end, I created several plots for visualizing one-class SVM results.
