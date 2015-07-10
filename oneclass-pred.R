#' class1 function to compute classifier for one-class SVM
#' 
#' @param newxy new set of coordinates for x, y
#' @param dat data used to train SVM
#' @param kernel kernel specification 
class1 <- function(newxy, kernel, dat = NULL, svm1 = NULL, type = "one-classification" ) {
    if(is.null(svm1)) {
        svm1 <- svm(dat, kernel = kernel, type = type, scale = F)
    }

    # Extract output
    SV <- svm1$SV
    gamma1 <- svm1$gamma
    rho <- svm1$rho
    coefs <- svm1$coefs

    # Get relevant kernel
    if(kernel == "radial") {
	# Compute norm of differences
    	kern <- list()
        for(i in 1 : nrow(newxy)) {
           kern[[i]] <- rowSums(sweep(SV, 2, newxy[i, ], "-")^2)

	}
	diffs <- unlist(kern)
	# Compute kernel
	kernel1 <-  exp(-gamma1 * diffs)
    } else if(kernel == "linear") {
	# Inner product
    	kern <- list()
        for(i in 1 : nrow(newxy)) {
           kern[[i]] <- rowSums(sweep(SV, 2, newxy[i, ], "*"))

	}
	kernel1 <- unlist(kern)
    }
    # Compute prediction
    coefs1 <- rep(coefs, nrow(newxy))
    val <- matrix(coefs1 *  kernel1, nrow = nrow(newxy), byrow = T)
    val <- rowSums(val) - rho	
    # Get sign, is normal sample?
    out <- sign(val) == 1

    return(out)
}


