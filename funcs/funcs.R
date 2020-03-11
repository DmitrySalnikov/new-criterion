path = '/home/d/1/new_criteria'

library(stringr)
source(paste(path, 'funcs', 'distributions.R', sep = '/'))

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

K <- function(X, Y, A, only_positive) {
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

Power <- function(data_path, only_positive = FALSE, n = 50, alpha = 0.05) {
  n <<- n 
  setwd(data_path)
  res <- rowMeans(sapply(str_sort(dir(), numeric = TRUE), function(RDSname) {
    print(RDSname)
    resRDS <- readRDS(RDSname)
    
    Z <- resRDS[['Z']]
    X <- Z[1:n]
    Y <- Z[(n+1):(2*n)]
    perm <- resRDS[['Z.perm']]
    A <- resRDS[['A']]
  
    stat <- K(X, Y, A, only_positive)
    stat <- rbind(stat, t(apply(perm, 1, function(Zp) { K(Zp[1:n], Zp[(n+1):(2*n)], A, only_positive) })))
  
    c(rowMeans(apply(stat[-1,], 1, function(x) x > stat[1,])),
      wilcox.test = wilcox.test(X, Y)$p.value,
      ks.test     = ks.test(X, Y)$p.value
    ) < alpha
  }))
  
  print(res)
  
  distribution <- tail(strsplit(data_path, '/')[[1]], 3)[1]
  parameter <- tail(strsplit(data_path, '/')[[1]], 3)[2]
  details <- paste(strsplit(tail(strsplit(data_path, '/')[[1]], 3)[3], ',')[[1]][1:3], collapse = ',')
  params <- paste0(paste(strsplit(tail(strsplit(data_path, '/')[[1]], 3)[3], ',')[[1]][4:5], collapse = ','), '.RDS')
  if (!dir.exists(paste(path, 'res', distribution, sep = '/'))) dir.create(paste(path, 'res', distribution, sep = '/'))
  if (!dir.exists(paste(path, 'res', distribution, parameter, sep = '/'))) dir.create(paste(path, 'res', distribution, parameter, sep = '/'))
  if (!dir.exists(paste(path, 'res', distribution, parameter, details, sep = '/'))) dir.create(paste(path, 'res', distribution, parameter, details, sep = '/'))
  res_name <- paste(path, 'res', distribution, parameter, details, params, sep = '/')
  saveRDS(res, res_name)
  
  NULL
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
