library(VaRES)
library(rmutil)

rloglaplace <- function(n, m = 0, s = 1) {
  exp(rlaplace(n, m, s))
}

dloglaplace <- function(x, m = 0, s = 1, log = FALSE) {
  probabilities <- exp( -abs(log(x) - m) / s ) / 2 / s / x
  if (log) log(probabilities) else probabilities
}

ploglaplace <- function(q, m = 0, s = 1, lower.tail = TRUE, log.p = FALSE) {
  probabilities <- 0.5 * ( 1 + sign(log(q) - m) * (1 - exp(-abs(log(q) - m) / s)) )
  if (!lower.tail) probabilities <- 1 - probabilities
  if (log.p) probabilities <- log(probabilities)
  probabilities
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