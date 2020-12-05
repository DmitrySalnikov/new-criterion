L2.test.stat <- function(x, y) {
  sum(sapply(x, function(x.i) { log(1 + (x.i - y)^2) }))
}

L2.modified.test.stat <- function(x, y, n.x, n.y, L2) {
  diff.x <- 0
  for (i in 1:(n.x - 1)) {
    for (j in (i + 1):(n.x)) {
      diff.x <- diff.x + log(1 + (x[i]-x[j])^2)
    }
  }

  diff.y <- 0
  for (i in 1:(n.y - 1)) {
    for (j in (i + 1):(n.y)) {
      diff.y <- diff.y + log(1 + (y[i]-y[j])^2)
    }
  }

  L2 / n.x / n.y - diff.x / n.x / (n.x-1) - diff.y / n.y / (n.y-1)
}

n <- 100
alpha <- 0.05
K <- 1000000
start.time <- Sys.time()
options(digits=22)
a <- apply(
  sapply(1:K, function(i) {
    if (i %% 10000 == 0) {
      print(paste0(i, ', ', Sys.time() - start.time))
    }
    x <- rnorm(n, 0, 0.1)
    y <- rnorm(n, 0, 0.1)
    L2 <- L2.test.stat(x, y)
    c(
      L2,
      L2.modified.test.stat(x, y, n, n, L2)
    )
  }
  ), MARGIN = 1, function(row) { quantile(row, 1-alpha) } )
a
print(a)