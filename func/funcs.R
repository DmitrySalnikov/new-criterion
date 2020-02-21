library(VaRES)
library(rmutil)

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

log.likelyhood <- function(par, x, distribution) {
  switch (distribution,
    norm = {
      -sum(dnorm(x, mean = mean(x), sd = sd(x), log = TRUE))
    }, cauchy = {
      -sum(dcauchy(x, location = par[1], scale = par[2], log = TRUE))
    }, logcauchy = {
      -sum(dlogcauchy(x, mu = par[1], sigma = par[2], log = TRUE))
    }, levy = {
      -sum(dlevy(x, m = par[1], s = par[2], log = TRUE))
    }, {
      print('unknown distribution')
      return()
    })
}

min.log.likelyhood <- function(x, distribution, n_start = 1) {
  if (distribution == 'norm') {
    return(log.likelyhood(c(mean(x), sd(x)), x, 'norm'))
  }

  distribution0 = distribution
  if (distribution == 'logcauchy') {
    y = x
    x = log(x)
    distribution = 'cauchy'
  }

  res <- matrix(ncol = 5, nrow = n_start)
  lower <- c(-Inf, 1e-6)
  upper <- c(Inf, Inf)

  switch (distribution,
    cauchy = {
      par1.estim <- mean(x, trim = 0.24)
      par2.estim <- IQR(x) / 2
      res[, 1] <- c(par1.estim, as.numeric(replicate(n_start - 1, par1.estim + runif(1, -2*par2.estim, 2*par2.estim))))
      res[, 2] <- c(par2.estim, as.numeric(replicate(n_start - 1, runif(1, 0, 2*par2.estim) + 1e-6)))
    }, levy = {
      par1.max <- min(x)
      x.as.gamma <- 1 / (x[x != par1.max] - par1.max)
      par2.estim <- max(1e-6, 2*(n-1)*(n-2) / ((n-1) * sum(x.as.gamma * log(x.as.gamma)) - sum(log(x.as.gamma)) * sum(x.as.gamma)), na.rm = TRUE)
      res[, 1] <- c(par1.max - 2*par2.estim, as.numeric(replicate(n_start - 1, par1.max + runif(1, -4*par2.estim, 0))))
      res[, 2] <- c(par2.estim, as.numeric(replicate(n_start - 1, runif(1, 0, 2 * par2.estim) + 1e-6)))
      upper <- c(par1.max - 1e-6, Inf)
    }, {
      print('unknown distribution')
      return()
    })

  for (i in 1:n_start) {
    tmp <- optim(res[i, 1:2], log.likelyhood, method = "L-BFGS-B", lower = lower, upper = upper,
                 x = x, distribution = distribution)
    res[i, 3:5] <- c(tmp$par, tmp$value)
  }

  min.idx <- which.min(res[, 5] == min(res[, 5]))

  if (distribution0 == 'logcauchy') {
    return(log.likelyhood(res[min.idx, 3:4], y, 'logcauchy'))
  }

  return(res[min.idx, 5])
}

likelyhood.test.stat <- function(X, Y, distribution) {
  -min.log.likelyhood(X, distribution) - min.log.likelyhood(Y, distribution)
}

K <- function(Z, A, logcauchy) {
  X <- Z[1:n]
  Y <- Z[(n+1):(2*n)]

  tmp <- vector()
  for (y in Y) {
    tmp <- c(tmp, abs(y - X))
  }
  tmpA <- tmp / A

  res <- c(
    L05 = sum(log(1 + tmp**.5)),
    L05C = sum(log(1 + tmpA**.5)),
    L1 = sum(log(1 + tmp)),
    L1C = sum(log(1 + tmpA)),
    L2 = sum(log(1 + tmp**2)),
    L2C = sum(log(1 + tmpA**2)),
    LLn = likelyhood.test.stat(X, Y, 'norm'),
    LLc = likelyhood.test.stat(X, Y, 'cauchy'),
    LLl = likelyhood.test.stat(X, Y, 'levy')
  )
  if (logcauchy) {
    res <- c(res, LLlc = likelyhood.test.stat(X, Y, 'logcauchy'))
  }
  res
}

Power <- function(Zd, exact = FALSE, logcauchy = FALSE) {
  print("step")
  rowMeans(apply(Zd,1,function(Z) {
    A <- 0
    for (i in 1:(2*n - 1))
      for (j in (i + 1):(2*n))
        A <- A + abs(Z[i] - Z[j])
    A <- A/(n * (2*n - 1))

    stat <- K(Z, A, logcauchy)

    perm <- if (exact) exact_perm(Z[1:5], Z[6:10]) else t(replicate(D,sample(Z)))
    stat <- rbind(stat, t(apply(perm, 1, function(Zp) { K(Zp, A, logcauchy) })))

    res <- c(rowMeans(apply(stat[-1,], 1, function(x) x > stat[1,])),
      wilcox.test = wilcox.test(Z[1:n], Z[(n+1):(2*n)])$p.value,
      ks.test     = ks.test(Z[1:n], Z[(n+1):(2*n)])$p.value
    ) < alpha
  }))
}

MakeTable <- function(idx1 = vector(), with_F1 = FALSE) {
  if (length(idx1)) res <- res[,c(1,2,idx1)]
  if (!with_F1) res <- res[,-1]

  cat('% ', file = paste0(tables, tname, '.tex'))
  write.table(res, paste0(tables, tname, '.tex'),
              quote = F, sep = ' & ', eol = ' \\\\\n',
              row.names = F, col.names = T, append = TRUE)
  write('\\hline', paste0(tables, tname, '.tex'), append = TRUE)
}

rNormCauchy <- function(n,perc,mu,sigma) {
  rb <- rbinom(n = n,size = 1, prob = perc)
  nc <- sum(rb)
  res <- vector(length = n)
  res[rb==1] <- rcauchy(nc,mu,sigma)
  res[rb==0] <- rnorm(n-nc,mu,sigma)
  res
}

T1 <- function(X, Y, X.center, Y.center) {
  if (X.center > Y.center) {
    t <- X
    X <- Y
    Y <- t
  }

  Z.center <- mean.var(c(X,Y))[1]
  X.plus <- X - Z.center
  X.plus <- X.plus[X.plus > 0]
  Y.minus <- Z.center - Y
  Y.minus <- Y.minus[Y.minus > 0]

  -(sum(log(1 + X.plus)) + sum(log(1 + Y.minus)))
}

G <- function(Z) {
  log(1 + Z**2)
}

exact_perm <- function(X, Y) {
  perms <- c(X,Y)

  for (i in 1:5)
    for (j in 1:5) {
      x <- X
      y <- Y
      tmp <- x[i]
      x[i] <- y[j]
      y[j] <- tmp
      perms <- rbind(perms, c(x,y))
    }

  i <- c(1,2,1,2)
  while(TRUE) {
    x <- X
    y <- Y
    tmp <- x[c(i[1],i[2])]
    x[c(i[1],i[2])] <- y[c(i[3],i[4])]
    y[c(i[3],i[4])] <- tmp
    perms <- rbind(perms, c(x,y))

    if (i[4] > 4) {
      if (i[3] > 3) {
        if (i[2] > 4) {
          if (i[1] > 3)
            break
          else {
            i[1] <- i[1]+1
            i[2] <- i[1]+1
            i[3] <- 1
            i[4] <- 2
          }
        } else {
          i[2] <- i[2]+1
          i[3] <- 1
          i[4] <- 2
        }
      } else {
        i[3] <- i[3]+1
        i[4] <- i[3]+1
      }
    } else {
      i[4] <- i[4]+1
    }
  }

  perms
}

C <- function(X, Xmed) {
  sum(abs( X-Xmed )) / (n-1)
}

center_estim <- function(X) {
  w <- c(-0.052,0.3485,0.407,0.3485,-0.052)
  ord_stat <- X[round(c(0.13,0.4,0.5,0.6,0.87)*n, 0)]
  sum(ord_stat * w)
}
