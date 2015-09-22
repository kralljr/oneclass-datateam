
# Find training data
lf <- list.files()
train1 <- lf[substr(lf, 1, 4) == "trai" & substr(lf, 23, 23) != "2"]



for(i in 1 : length(train1)) {
    dat <- read.csv(train1[i], header = F, stringsAsFactors = F)

    app1 <- ifelse(i == 1, F, T)

    print(c(i, app1))
    write.table(dat1, sep = ",",file = "train.csv", append = app1, col.names = !app1, row.names = F)
}

