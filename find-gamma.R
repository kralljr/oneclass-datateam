library(ggplot2)
library(dplyr)
library(RColorBrewer)

#  Source error functions
source(file.path(dir2, "error-fun.R"))

# Load predictions
#load(file.path(dir1, "strand-validation-predictions.Rdata"))
load(file.path(dir1, "strand-test-predictions.Rdata"))

# Load validation data
#valid <- read.csv(file.path(dir1, "validation.csv"))
#truth <- valid$label

# Load test data
test <- read.csv(file.path(dir1, "test-names.csv"))
truth <- test$label

# Gammas used
#gam1 <- c(seq(.01, 1, length = 5), 1.5, 2, 2.5, 3, 5)



# Function to get errors
errfun <- function(dat, truthi, type) {
  # get gammas
  gam1 <- as.numeric(substring(colnames(dat), 2))
  
  # Get type 1 and type 2 errors for each gamma (nu = 0.0001)
  errout <- matrix(nrow = ncol(dat), ncol = 3)
  for(i in 1 : ncol(dat)) {
    errout[i, 2] <- type1(truth, dat[, i]) 
    errout[i, 3] <- type2(truth, dat[, i]) 
  }
  errout[, 1] <- gam1

  errout <- data.frame(errout, rep(type, nrow(errout)))
  # format output
  colnames(errout) <- c("Gamma", "Type1", "Type2", "type")

  errout
}


rf1 <- errfun(reducedFeatures, truth, "reduced")
af1 <- errfun(allFeatures, truth, "all")


errout <- full_join(rf1, af1)


# plot of sens/1-spec
errout2 <- data.frame(errout)
errout2$Type2 <- 1 - errout2$Type2
colnames(errout2)[2:3] <- c("type1", "tp")

cols <- brewer.pal(4, "Set1")[2:3]


gplot1 <- function(typekeep, cols) {
  dat <- dplyr::filter(errout2, type %in% typekeep)
  
  g1 <- ggplot(dat, aes(x = type1, y = tp, colour = type)) + geom_point(size = 3) +
  geom_line() + ylab("Detection rate") + 
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_abline(intercept = 0, slope = 1, colour = "grey") +
  scale_colour_manual(name = "", values = cols) +
  xlab("False Alarm rate") + 
  theme_bw() + theme(text = element_text(size = 18)) 
  
  if(length(typekeep) == 1) {
    g1 <- g1 + theme(legend.position = "none")
  
  }
  g1

}


names1 <- c("ROC-reduced","ROC-all", "ROC-reduced-all") 
types1 <- list("reduced", "all", c("reduced", "all"))
cols1 <- list(cols[1], cols[2], cols)
for(i in 1 : 3) {
  
  g1 <- gplot1(types1[[i]], cols1[[i]])
  
  ggsave(filename = paste0(names1[i], ".png"), plot = g1)
  graphics.off()

  ggsave(filename = paste0(names1[i], ".pdf"), plot = g1)
  graphics.off()
}
