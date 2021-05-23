logXX <- function(X) {
  sum(as.vector(sapply(X, function(X.i) {
    log(1 + (X.i - X) ** 2)
  })))
}

logYY <- function(Y) {
  sum(as.vector(sapply(Y, function(Y.i) {
    log(1 + (Y.i - Y) ** 2)
  })))
}

logXY <- function(X, Y) {
  sum(as.vector(sapply(X, function(X.i) {
    log(1 + (X.i - Y) ** 2)
  })))
}

J0 <- function(XX, YY) {
  n = length(X)
  
  (XX + YY) / n / (n - 1)
}

J1 <- function(XY, XX, YY) {
  n = length(X)
  
  2 * XY / n - XX / (n - 1) - YY / (n - 1)
}

power_estimation <- function(X, Y) {
  n = length(X)
  
  XY <- logXY(X, Y)
  XX <- logXX(X)
  YY <- logYY(Y)
  J0 <- J0(XX, YY)
  J1 <- J1(XY, XX, YY)
  
  q <- qnorm(0.975)
  additive <- sqrt(3 * J1 / J0)
  power <- pnorm(q - additive, lower.tail = F) + pnorm(-q - additive)
  
  list(power=power, J0=J0, J1=J1, XX=XX/n/(n-1), YY=YY/n/(n-1), XY = 2*XY/n**2)
}

generate_samples_mean_difference <- function(distribution, n, M, h) {
  par1 = c(0, 1)
  par2 = c(h / sqrt(n), 1)
  set.seed(500)
  X.set <- get(paste0('r', distribution))(n * M, par1[1], par1[2])
  Y.set <- get(paste0('r', distribution))(n * M, par2[1], par2[2])
  dim(X.set) <- c(M, n)
  dim(Y.set) <- c(M, n)
  list(X = X.set, Y = Y.set)
}

alpha <- 0.05
n <- 1000
M <- 1000
res <- c()
for (h in c(1,2,3,5,7,9)) {
  X <- rcauchy(n, 0, 1)
  Y <- rcauchy(n, h / sqrt(n), 1)
  estim <- power_estimation(X, Y)
  print(round(c(XY = logXY(X, Y), XX = logXX(X), YY = logYY(Y), 
                J0 = J0(X, Y), J1 = J1(X, Y)), 3))
    
  res <- c(res, estim)
}
res

####### norm case

norm_power_estimation <- function(h, alpha = 0.05) {
  J0 <- 0.81
  J1 <- function(h) { h ** 2 / 4.4 }
  
  q <- qnorm(1 - alpha / 2)
  additive <- sqrt(3 * J1(h) / J0)
  pnorm(q - additive, lower.tail = F) + pnorm(-q - additive)
}

res <- c()
for (h in 1:5) { res <- c(res, norm_power_estimation(h)) }
res

