
typerr <- seq(0, 1, length = 20)

w1 <- 0.4
out <- matrix(nrow = length(typerr), ncol =length( typerr))
for(i in 1 : length(typerr)) {
  for(j in 1 : length(typerr)) {
    out[i, j] <- w1 * typerr[i] + (1 - w1) * typerr[j] 
  }
}

rtyperr <- round(typerr, 2)
rownames(out) <- paste0("type1=", rtyperr[1 : nrow(out)])
colnames(out) <- paste0("type2=", rtyperr)

round(out[1 : 5, ], 2)
