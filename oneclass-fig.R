# source class1 function
source("~/Documents/Work/STRAND/oneclass-pred.R")

# Change this directory path to where your data are stored
mylib <- "~/Documents/Work/STRAND"



# Load libraries
#library(e1071)
#library(dplyr)

# Read in data
#postal1 <- read.csv(file.path(mylib, "train.1.1.txt"))
#postal2 <- read.csv(file.path(mylib, "train.2.2.txt"))



# Load libraries
library(e1071)
library(dplyr)
library(mvtnorm)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(devtools)

# May need to run this:
#install_github("ropensci/plotly")
#library(plotly)




#################
# Simulate data

# Specify parameters
N <- 500
mean1 <- 1 * c(10, 10)
sig2 <- 1
c1 <- 0.6
corr <- cbind(c(1, c1), c(c1, 1))
sigma1 <- sig2 * corr

# set seed
set.seed(10)
# get multivariate normal
x <- rmvnorm(N, mean = mean1, sigma = sigma1)








#################
# Apply one-class SVM with radial kernel
svm_model <- svm(x, kernel = "radial", scale = F, type = "one-classification")

# Gamma at default
# Nu at default


# Get predictions
pred2 <- predict(svm_model)
xT <- x[pred2, ]

# Rerun model with these closely clustered points

# Apply one-class SVM with kernel
svm_model <- svm(xT, kernel = "radial", scale = F, type = "one-classification", nu = 0.01)

# Get predictions
xT <- xT[predict(svm_model), ]













##################
# Get grid to get boundary
# Sample grid points near boundary
N <- 400
x1 <- seq(min(x), max(x), length = N)
x2 <- x1
xs <- cbind(rep(x1, N), rep(x2, each = N))

# Classify grid points
c1 <- class1(xs, "radial", svm1 = svm_model)

# Get convex hull
true <- xs[c1 == 1, ]
b1 <- true[chull(true), ]
b1 <- rbind(b1, b1[1, ])
b1 <- data.frame(b1)


# Rename columns
colnames(xT) <- c("x", "y")
colnames(b1) <- c("x", "y")



# Make sure away from boundary
d1 <- as.matrix(dist(rbind(xT, b1)))
d1 <- d1[(1 : nrow(xT)), (nrow(xT) + 1) : (nrow(b1) + nrow(xT))]
d1 <- apply(d1, 1, min)
xT2 <- xT[which(d1 > 0.01),] 

# Sample subset
set.seed(10)
samps <- sample(seq(1, nrow(xT2)), 50)
newdat <- data.frame(xT2[samps, ])

newdatxT <- xT2[-samps,]
newdatxT <- data.frame(newdatxT[1 : 5, ])

# Get exterior points
xF <- x[!pred2, ]

# Make sure away from boundary
colnames(xF) <- c("x", "y")
d2 <- as.matrix(dist(rbind(xF, b1)))
 d2 <- d2[(1 : nrow(xF)), (nrow(xF) + 1) : (nrow(b1) + nrow(xF))]
d2 <- apply(d2, 1, min)
xF <- xF[which(d2 > 0.3), ]


set.seed(10)
samps <- sample(seq(1, nrow(xF)), 10)
newdat2 <- data.frame(xF[samps, ])


# Fix column names for all data
colnames(newdat2) <- c("z1", "z2")
colnames(newdat) <- c("x1", "x2")
colnames(b1) <- c("b1", "b2")
colnames(newdatxT) <- c("xt1", "xt2")








#############
# Create figures

# Upper and lower limits
min1 <- 8
max1 <- 12


# Specify colors
cols2 <- brewer.pal(3, "Dark2")[1 : 2]

# Why points outside of circle?  Need more?
# Why points outside of circle?  Need more?
# Plot training points (not anomalous)
g1 <- ggplot(newdat, aes(x = x1, y = x2)) + 
	xlim(min1, max1) + ylim(min1, max1) +
	geom_point(size = 5) +
	theme_bw() +
	xlab("Feature 1") + ylab("Feature 2") +
	theme(text = element_text(size = 20) 
    	,plot.background = element_blank()
   	,panel.grid.major = element_blank()
   	,panel.grid.minor = element_blank()
   	,panel.border = element_blank()
   	,axis.text = element_blank()
   	,axis.ticks = element_blank()
  	)



# Plot boundary
g2 <- g1 + geom_path(data = b1, aes(x = b1, y = b2), color = cols2[1], size = 5) +
	theme_bw() +
	xlab("Feature 1") + ylab("Feature 2") +
	theme(text = element_text(size = 20)
    	,plot.background = element_blank()
   	,panel.grid.major = element_blank()
   	,panel.grid.minor = element_blank()
   	,panel.border = element_blank()
   	,axis.text = element_blank()
   	,axis.ticks = element_blank()
   	)
   	
# Plot validation points
g3 <- ggplot(newdat, aes(x = x1, y = x2)) + 
	xlim(min1, max1) + ylim(min1, max1) +
	geom_point(data = newdat2, aes(x = z1, y = z2), colour = cols2[2], 
	size = 5, shape = 8) +
	geom_point(data = newdatxT, aes(x = xt1, y = xt2), colour = cols2[2], size = 5) +
	geom_path(data = b1, aes(x = b1, y = b2), color = cols2[1], size = 5) +
	theme_bw() +
	xlab("Feature 1") + ylab("Feature 2") +
	theme(text = element_text(size = 20) 
    	,plot.background = element_blank()
   	,panel.grid.major = element_blank()
   	,panel.grid.minor = element_blank()
   	,panel.border = element_blank()
   	,axis.text = element_blank()
   	,axis.ticks = element_blank()
  	)

# Plot discovered anomalous points over original points
g4 <- g2 + geom_point(data = newdat2, aes(x = z1, y = z2), colour = cols2[2], 
	size = 5, shape = 8) +
	geom_point(data = newdatxT, aes(x = xt1, y = xt2), colour = cols2[2], size = 5) +
	theme_bw() +
	xlab("Feature 1") + ylab("Feature 2") +
	theme(text = element_text(size = 20)
    	,plot.background = element_blank()
   	,panel.grid.major = element_blank()
   	,panel.grid.minor = element_blank()
   	,panel.border = element_blank()
   	,axis.text = element_blank()
   	,axis.ticks = element_blank()
   	)





png("oneclass-viz.png", height = 500, width = 1400)
grid.arrange(g1, g2, g3, g4, nrow = 1)
dev.off()


# To do: try 3-d plot

