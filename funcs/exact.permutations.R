next.perm.idx <- function(perm.idx, k, n) {
  perm.idx
  for (i in k:1) {
    if (perm.idx[i] < n+i-k) {
      perm.idx[i] = perm.idx[i] + 1
      if (i != k) {
        for (j in (i+1):k) {
          perm.idx[j] = perm.idx[j-1] + 1
        }
      }
      return (perm.idx)
    }
  }
  NULL
}

generate.perm.idx <- function(k, n) {
  idx <- list(1:k)
  i <- 1
  while (TRUE) {
    next.idx <- next.perm.idx(idx[[i]], k, n)
    if (!is.null(next.idx)) {
      i <- i + 1
      idx[[i]] <- next.idx
    } else {
      break
    }
  }
  idx
}

exact.permutations <- function(x, y, n.x = NULL, n.y = NULL) {
  if (is.null(n.x)) n.x <- length(x)
  if (is.null(n.y)) n.y <- length(y)
  
  n <- if (n.x == n.y) n.x%/%2 else min(n.x, n.y)
  perms <- c(x, y)
  for (k in 1:n) {
    x.idx <- generate.perm.idx(k, n.x)
    y.idx <- generate.perm.idx(k, n.y)
    for (x.i in x.idx) {
      for (y.i in y.idx) {
        perms <- cbind(perms, c(x[-x.i], y[y.i], x[x.i], y[-y.i]))
      }
    }
  }
  perms
}

n.exact.perms <-function(n.x, n.y, n.z) {
  n <- factorial(n.z) / factorial(n.x) / factorial(n.y)
  if (n.x == n.y) n / 2 else n
}