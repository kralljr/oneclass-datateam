library(caret)
library(e1071)

str(svmBag)
svmBag$fit
svmBag$pred
svmBag$aggregate

#Define custom bagging svm function using svm package of our choosing
bsvm.fit <- function (x, y, ...)
     {
         loadNamespace("e1071")
         out <- e1071::libsvm(as.matrix(x), y, prob.model = is.factor(y),
             ...)
         out
     }

bsvm.pred <- function (object, x)
     {
         if (is.character(lev(object))) {
             out <- predict(object, as.matrix(x), type = "probabilities")
             colnames(out) <- lev(object)
             rownames(out) <- NULL
         }
         else out <- predict(object, as.matrix(x))[, 1]
         out
     }

bsvm.aggregate <- function (x, type = "class")
     {
         if (is.matrix(x[[1]]) | is.data.frame(x[[1]])) {
             pooled <- x[[1]] & NA
             classes <- colnames(pooled)
             for (i in 1:ncol(pooled)) {
                 tmp <- lapply(x, function(y, col) y[, col], col = i)
                 tmp <- do.call("rbind", tmp)
                 pooled[, i] <- apply(tmp, 2, median)
             }
             if (type == "class") {
                 out <- factor(classes[apply(pooled, 1, which.max)],
                     levels = classes)
             }
             else out <- as.data.frame(pooled)
         }
         else {
             x <- matrix(unlist(x), ncol = length(x))
             out <- apply(x, 1, median)
         }
         out
     }

#Define "number" to be number of folds desired
#Define selectionFunction to be "best", "oneSE", or "tolerance"
ctrl <- trainControl(method = "cv", number = 10, selectionFunction = "oneSE")

#Creating bagging control object
bagctrl <- bagControl(fit = bsvm.fit,
                      predict = bsvm.pred,
                      aggregate = bsvm.aggregate)


set.seed(300)
svmbag <- train(default ~ ., data = _______, "bag", trControl = ctrl, bagControl = bagctrl)
svmbag