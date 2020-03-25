path = '/home/d/1/new_criteria'

source(paste(path, 'funcs', 'distributions.R', sep = '/'))
source(paste(path, 'funcs', 'exact.permutations.R', sep = '/'))

DISTRIBUTIONS <- c('norm', 'laplace', 'cauchy', 'levy')
LOG.DISTRIBUTIONS <- c('loglaplace', 'logcauchy')

MeanAD <- function(x, center, power = 1) {
  mean(abs(x - center)**power)
}

sd.center <- function(x, center) {
  sqrt(sum((x - center)**2) / (length(x) - 1))
}

get.A <- function(z) {
  A <- 0
  for (i in 1:(2*n - 1)) {
    for (j in (i + 1):(2*n)) {
      A <- A + abs(z[i] - z[j])
    }
  }
  
  A / (n * (2*n - 1))
}

log.likelyhood <- function(par, x, distribution) {
  density_function <- get(paste0('d', distribution))
  
  -sum(density_function(x, par[1], par[2], log = TRUE))
}

find.distribution.par <- function(x, distribution) {
  if (distribution == 'norm') {
    return(c(mean(x), sd(x)))
  }
  
  if (distribution %in% LOG.DISTRIBUTIONS) {
    x = log(x)
    distribution = substring(distribution, 4)
  }

  lower <- c(-Inf, 1e-6)
  upper <- c(Inf, Inf)

  switch (distribution,
    cauchy = {
      par1.estim <- mean(x, trim = 0.24)
      par2.estim <- IQR(x) / 2
    }, laplace = {
      par1.estim <- median(x)
      par2.estim <- MeanAD(x, par1.estim)
    }, levy = {
      par1.max <- min(x)
      x.as.gamma <- 1 / (x[x != par1.max] - par1.max)
      par2.estim <- max(1e-6, 2*(n-1)*(n-2) / ((n-1) * sum(x.as.gamma * log(x.as.gamma)) - sum(log(x.as.gamma)) * sum(x.as.gamma)), na.rm = TRUE)
      par1.estim <- par1.max - 2*par2.estim
      upper <- c(par1.estim - 1e-6, Inf)
    }, {
      print('unknown distribution')
      return()
    })
  par.estim <- c(par1.estim, par2.estim)

  optim(par.estim, log.likelyhood, method = "L-BFGS-B", lower = lower, upper = upper,
        x = x, distribution = distribution)$par
}

likelyhood.test.stat <- function(x, y, par.x, par.y, distribution) {
  -log.likelyhood(par.x, x, distribution) - log.likelyhood(par.y, y, distribution)
}

L.test <- function(x, y, z, A, permutations) {
  diff <- sapply(x, function(x.i) { abs(x.i - y) })
  dim(diff) <- NULL
  diff.A <- diff / A
  stat0 <- c(
    L05 = sum(log(1 + diff**.5)),
    L05C = sum(log(1 + diff.A**.5)),
    L1 = sum(log(1 + diff)),
    L1C = sum(log(1 + diff.A)),
    L2 = sum(log(1 + diff**2)),
    L2C = sum(log(1 + diff.A**2))
  )
  
  stat <- t(apply(permutations, 1, function(p) { 
    x <- z[p[1:n]]
    y <- z[p[(n+1):(2*n)]]
    diff <- sapply(x, function(x.i) { abs(x.i - y) })
    dim(diff) <- NULL
    diff.A <- diff / A
    c(
      L05 = sum(log(1 + diff**.5)),
      L05C = sum(log(1 + diff.A**.5)),
      L1 = sum(log(1 + diff)),
      L1C = sum(log(1 + diff.A)),
      L2 = sum(log(1 + diff**2)),
      L2C = sum(log(1 + diff.A**2))
    )
  }))
  
  rowMeans(apply(stat, 1, function(s) { s > stat0 } ))
}

LL.norm.var.equal.test <- function(x, y, z, permutations) {
  sd.z <- sd(z)
  par.x <- c(mean(x), sd.z)
  par.y <- c(mean(y), sd.z)
  stat0 <- likelyhood.test.stat(x, y, par.x, par.y, 'norm')
  stat <- apply(permutations, 1, function(p) { 
    x <- z[p[1:n]]
    y <- z[p[(n+1):(2*n)]]
    par.x <- c(mean(x), sd.z)
    par.y <- c(mean(y), sd.z)
    likelyhood.test.stat(x, y, par.x, par.y, 'norm') 
  })
  
  mean(stat > stat0)
}

LL.norm.mean.equal.test <- function(x, y, z, permutations) {
  mean.z <- mean(z)
  par.x <- c(mean.z, sd.center(x, mean.z))
  par.y <- c(mean.z, sd.center(y, mean.z))
  stat0 <- likelyhood.test.stat(x, y, par.x, par.y, 'norm')
  stat <- apply(permutations, 1, function(p) { 
    x <- z[p[1:n]]
    y <- z[p[(n+1):(2*n)]]
    par.x <- c(mean.z, sd.center(x, mean.z))
    par.y <- c(mean.z, sd.center(y, mean.z))
    likelyhood.test.stat(x, y, par.x, par.y, 'norm') 
  })
  
  mean(stat > stat0)
}

LL.test <- function(x, y, z, distribution, permutations, var.equal = FALSE, mean.equal = FALSE) {
  if (distribution == 'norm' && var.equal == TRUE) {
    return(LL.norm.var.equal.test(x, y, z, permutations))
  }
  if (distribution == 'norm' && mean.equal == TRUE) {
    return(LL.norm.mean.equal.test(x, y, z, permutations))
  }
  
  par.x <- find.distribution.par(x, distribution)
  par.y <- find.distribution.par(y, distribution)
  stat0 <- likelyhood.test.stat(x, y, par.x, par.y, distribution)

  stat <- apply(permutations, 1, function(p) { 
    x <- z[p[1:n]]
    y <- z[p[(n+1):(2*n)]]
    par.x <- find.distribution.par(x, distribution)
    par.y <- find.distribution.par(y, distribution)
    likelyhood.test.stat(x, y, par.x, par.y, distribution) 
  })
  
  mean(stat > stat0)
}

Power <- function(distribution, par2, type, par1 = c(0, 1), n = 50, M = 10000, D = 1600, alpha = 0.05) {
  n <<- n
  
  set.seed(500)
  X.set <- get(paste0('r', distribution))(n * M, par1[1], par1[2])
  Y.set <- get(paste0('r', distribution))(n * M, par2[1], par2[2])
  permutations.set <- replicate(M, t(replicate(D, sample(1:(2*n)))), simplify = FALSE)
  dim(X.set) <- c(M, n)
  dim(Y.set) <- c(M, n)
  Z.set <- cbind(X.set, Y.set)
  A.set <- apply(Z.set, 1, get.A)
  
  res <- rowMeans(sapply(1:M, function(i) {
    print(i)
    X <- X.set[i, ]
    Y <- Y.set[i, ]
    Z <- Z.set[i, ]
    permutations <- permutations.set[[i]]
    A <- A.set[i]
  
    res <- c(
      L.test(X, Y, Z, A, permutations),
      LLnorm = LL.test(X, Y, Z, 'norm', permutations),
      LLnorm.var.eq = LL.test(X, Y, Z, 'norm', permutations, var.equal = TRUE),
      LLnorm.mean.eq = LL.test(X, Y, Z, 'norm', permutations, mean.equal = TRUE),
      LLcauchy = LL.test(X, Y, Z, 'cauchy', permutations),
      LLlaplace = LL.test(X, Y, Z, 'laplace', permutations),
      LLlevy = LL.test(X, Y, Z, 'levy', permutations)
    )
    if (distribution %in% LOG.DISTRIBUTIONS) {
      res <- c(res,
         LLlogcauchy = LL.test(X, Y, Z, 'logcauchy', permutations),
         LLloglaplace = LL.test(X, Y, Z, 'loglaplace', permutations)
      )
    }
    
    c(res,
      wilcox.test = wilcox.test(X, Y)$p.value,
      ks.test     = ks.test(X, Y)$p.value
    ) < alpha
  }))
  
  print(res)
  
  details <- paste0('par2=(', par2[1], ',', par2[2], '),n=', n, ',M=', M, ',D=', D, '.RDS')
  if (!dir.exists(paste(path, 'res', distribution, sep = '/'))) dir.create(paste(path, 'res', distribution, sep = '/'))
  if (!dir.exists(paste(path, 'res', distribution, type, sep = '/'))) dir.create(paste(path, 'res', distribution, type, sep = '/'))
  res_name <- paste(path, 'res', distribution, type, details, sep = '/')
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
