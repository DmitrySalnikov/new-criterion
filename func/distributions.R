library(VaRES)
library(rmutil)

rlaplace <- function(n, m, b) {
  m + b * sqrt(2*rexp(n, 1)) * rnorm(n, 0, 1)
}

rloglaplace <- function(n, m, b) {
  exp(rlaplace(n, m, b))
}

rlogcauchy <- function(n, location = 0, scale = 1) {
  res <- exp(rcauchy(n, location, scale))
  idx.inf <- res > 1e153 | res == 0
  n.inf <- sum(idx.inf)
  while (n.inf > 0) {
    res[idx.inf] <- exp(rcauchy(n.inf, location, scale))
    idx.inf <- res > 1e153 | res == 0
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