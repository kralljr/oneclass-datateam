# Read in data
dat <- read.table("fv.uniq.txt", sep = ",")


# Get linear funciton of features
linfun <- 2 + 5 * dat[, 1]/ dat[, 2] + 0.5 * dat[, 3] + 
	exp(dat[, 5] * dat[, 4]) + 2 * dat[, 3]

# Get mean for logistic
expit <- function(x) {
	exp(x) / (1 + exp(x))
}
mean <- expit(linfun)

# Sample y
set.seed(56247)
y <- rbinom(length(mean), 1, mean)

# Label y
y <- factor(y, levels = c(0, 1), labels = c("Attack", "Normal"))
table(y) / length(y)