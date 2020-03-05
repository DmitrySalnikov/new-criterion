library(VaRES)
library(rmutil)

rloglaplace <- function(n, m, b) {
  res <- exp(rlaplace(n, m, b))
}

dloglaplace <- function(x, m, b, log = FALSE) {
  probabilities <- sapply(x, function(x.i) { exp( -abs(log(x.i) - m) / b ) / 2 / b / x.i })
  if (log) log(probabilities) else probabilities
}

rlogcauchy <- function(n, location = 0, scale = 1) {
  res <- exp(rcauchy(n, location, scale))
  idx.inf <- res > 1e145 | res == 0
  n.inf <- sum(idx.inf)
  while (n.inf > 0) {
    res[idx.inf] <- exp(rcauchy(n.inf, location, scale))
    idx.inf <- res > 1e145 | res == 0
    n.inf <- sum(idx.inf)
  }
  res
}

rnorm.cauchy <- function(n,mu,sigma,perc=0.05) {
  rb <- rbinom(n = n,size = 1, prob = perc)
  nc <- sum(rb)
  res <- vector(length = n)
  res[rb==1] <- rcauchy(nc,mu,sigma)
  res[rb==0] <- rnorm(n-nc,mu,sigma)
  res
}