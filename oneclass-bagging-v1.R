library(caret)
library(e1071)
library(dplyr)

#Call original svmBag functions
#str(svmBag)
#svmBag$fit
#svmBag$pred
#svmBag$aggregate

#Define custom bagging svm function using svm package of our choosing {fit, pred, aggregate}
bsvm.fit <- function (x, y, ...)
     {
         loadNamespace("e1071")
         out <- e1071::svm(as.matrix(x), y, prob.model = is.factor(y),
             ...)
         out
     }

bsvm.pred <- svmBag$pred

bsvm.aggregate <- svmBag$aggregate

#Define "number" to be number of folds desired
#Define selectionFunction to be "best", "oneSE", or "tolerance"
set.seed(300)
ctrl <- trainControl(method = "boot", number = 2, selectionFunction = "best")
?trainControl

#Creating bagging control object
bagctrl <- bagControl(fit = bsvm.fit,
                      predict = bsvm.pred,
                      aggregate = bsvm.aggregate)

#Fix data formatting
dat1$y <- as.numeric(dat1$y)
dat1$V4 <- as.factor(dat1$V4)
dat1$V5 <- as.factor(dat1$V5)

#Create a sample subset for faster testing runtimes
set.seed(300)
dat1_subset10 <- sample_n(dat1, 1000)

head(dat1_subset10)


#Evaluate bagged function
set.seed(300)
svmbag <- train(y ~., data = dat1_subset10, "bag", trControl = ctrl, bagControl = bagctrl)

#Warnings 11, 22, and 23 mean...?
warnings()

svmbag