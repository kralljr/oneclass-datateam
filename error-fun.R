# Error functions for SVM


# Function to get type 1 and type 2 errors
#' @param class1 classifications from SVM
#' @param valid validation data
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




# Type 1 error function
#' @param true true values
#' @param class1 classified values
type1 <- function(true, class1) {
    # which normal
    class2 <- class1[true]
  # which attack | normal
  x1 <- class2[!class2]
    t1 <- length(x1) / length(class2)
    t1
}





# Type 2 error function
#' @param true true values
#' @param class1 classified values
type2 <- function(true, class1) {
    # which attack
    class2 <- class1[!true]
  # which normal | attack
  x1 <- class2[class2]
    t1 <- length(x1) / length(class2)
    t1
}




# Weighted average of type 1 and 2 errors
#' @param true true values
#' @param class1 classified values
#' @param w weight between 0 and 1
mixtype <- function(true, class1, w = 0.5) {
    type1U <- type1(true, class1)
  type2U <- type2(true, class1)
    out <- (1 - w) * type1U + w * type2U
    return(out)

}

