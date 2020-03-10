path = '/home/d/1/new_criteria/'

source(paste0(path, 'funcs/exact.permutations.R'))
source(paste0(path, 'funcs/distributions.R'))

MeanAD <- function(x, center, power = 1) {
  mean(abs(x - center)**power)
}

log.likelyhood <- function(par, x, distribution) {
  switch (distribution,
    norm = {
      -sum(dnorm(x, mean = mean(x), sd = sd(x), log = TRUE))
    }, cauchy = {
      -sum(dcauchy(x, location = par[1], scale = par[2], log = TRUE))
    }, laplace = {
      -sum(dlaplace(x, m = par[1], s = par[2], log = TRUE))
    }, levy = {
      -sum(dlevy(x, m = par[1], s = par[2], log = TRUE))
    }, loglaplace = {
      -sum(dloglaplace(x, m = par[1], s = par[2], log = TRUE))
    }, logcauchy = {
      -sum(dlogcauchy(x, mu = par[1], sigma = par[2], log = TRUE))
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
  if (distribution == 'loglaplace') {
    y = x
    x = log(x)
    distribution = 'laplace'
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
    }, laplace = {
      par1.estim <- median(x)
      par2.estim <- MeanAD(x, par1.estim)
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
  if (distribution0 == 'loglaplace') {
    return(log.likelyhood(res[min.idx, 3:4], y, 'loglaplace'))
  }

  return(res[min.idx, 5])
}

likelyhood.test.stat <- function(X, Y, distribution) {
  -min.log.likelyhood(X, distribution) - min.log.likelyhood(Y, distribution)
}

K <- function(Z, A, only_positive) {
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
    LLnorm = likelyhood.test.stat(X, Y, 'norm'),
    LLcauchy = likelyhood.test.stat(X, Y, 'cauchy'),
    LLlaplace = likelyhood.test.stat(X, Y, 'laplace'),
    LLlevy = likelyhood.test.stat(X, Y, 'levy')
  )
  if (only_positive) {
    res <- c(res, 
      LLlogcauchy = likelyhood.test.stat(X, Y, 'logcauchy'),
      LLloglaplace = likelyhood.test.stat(X, Y, 'loglaplace')
    )
  }
  
  res
}

Power <- function(Zd, exact = FALSE, only_positive = FALSE) {
  print("step")
  rowMeans(apply(Zd,1,function(Z) {
    A <- 0
    for (i in 1:(2*n - 1))
      for (j in (i + 1):(2*n))
        A <- A + abs(Z[i] - Z[j])
    A <- A/(n * (2*n - 1))

    stat <- K(Z, A, only_positive)

    perm <- if (exact) t(exact.permutations(Z[1:5], Z[6:10])) else t(replicate(D,sample(Z)))
    stat <- rbind(stat, t(apply(perm, 1, function(Zp) { K(Zp, A, only_positive) })))

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
