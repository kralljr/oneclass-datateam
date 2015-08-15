# Read in data
dat <- read.table("fv.uniq.txt", sep = ",")

# Get linear funciton of features
linfun <- 2 + 5 * dat[, 1]/ dat[, 2] +	exp(dat[, 5] * dat[, 4]) + 2.5 * dat[, 3]

#linfun <- dat[, 3] / 100 + dat[, 4]


# Get mean for logistic
expit <- function(x) {
	exp(x) / (1 + exp(x))
}
mean <- expit(linfun)

# Sample y
set.seed(56247)
y <- rbinom(length(mean), 1, mean)
y <- as.logical(y)




# Try thresholding
yfun <- function(x) {
	#out <- 1 * ((x[1] < 20) & (x[2] < 100)) & x[3]  < 2
	out <-  (x[1] > 20 | x[2] > 100)
}

#y <- apply(dat, 1, yfun)




# Label y
#y <- factor(y, levels = c(0, 1), labels = c("Attack", "Normal"))
#table(y) / length(y)




# Test
dat1 <- data.frame(y, dat)
#glm1 <- glm(y ~ V1 + V2 + V3 + V4 + V5, data = dat1, family = "binomial")
#summary(glm1)$coef %>% round(., 2)
