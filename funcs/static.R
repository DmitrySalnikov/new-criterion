create.folder <- function(folder.path) {
  res <- folder.path[1]
  for (folder in folder.path[-1]) {
    res <- paste(res, folder, sep = '/')
    if (!dir.exists(res)) {
      dir.create(res)
    }
  }
  
  res
}

make.details <- function(par2, n, M, D, prefix, randomization) {
  exact <- n == 5
  if(exact) {
    D <- n.exact.perms(5, 5, 10)
  }
  details <- paste0(prefix, 'par2=(', par2[1], ',', par2[2], '),n=', n, ',M=', M, ',D=', D)
  if(exact) {
    details <- paste0(details, ',exact')
  }
  if(randomization) {
    details <- paste0(details, ',rand')
  }
  
  details
}

make.res.path <- function(distribution, type) {
  create.folder(c(path, 'res', distribution, type))
}

make.res.name <- function(distribution, type, par2, n, M, D, prefix, randomization) {
  details <- make.details(par2, n, M, D, prefix, randomization)
  res.path <- make.res.path(distribution, type)
  paste0(res.path, '/', details, '.RDS')
}

read.res <- function(distribution, type, par2, n = 50, M = 1000, D = 800, prefix = NULL, randomization = TRUE) {
  res.name <- make.res.name(distribution, type, par2, n, M, D, prefix, randomization)
  readRDS(res.name)
}

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
  density.function <- get(paste0('d', distribution))
  
  -sum(density.function(x, par[1], par[2], log = TRUE))
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
            upper <- c(par1.max - 1e-6, Inf)
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

phi <- function(T0, Tk) {
  Tk <- sort(Tk)
  k <- n.permutations - round(n.permutations * alpha)

  if (isTRUE(all.equal(T0, Tk[k]))) {
    Mp <- sum(sapply(Tk, function(Tkk) { !isTRUE(all.equal(Tkk, Tk[k])) & Tkk > Tk[k] } ))
    M0 <- sum(sapply(Tk, function(Tkk) { isTRUE(all.equal(Tk[k], Tkk)) } ))
    (n.permutations * alpha - Mp) / M0 
  } else {
    T0 > Tk[k]
  }
}

L.test <- function(x, y, z, A, permutations) {
  stat0 <- L.test.stat(x, y, A)
  
  stat <- t(apply(permutations, 1, function(z) { 
    x <- z[1:n]
    y <- z[(n+1):(2*n)]
    L.test.stat(x, y, A)
  }))
  
  if (randomization) {
    sapply(1:length(stat0), function(i) { phi(stat0[i], stat[, i]) } ) 
  } else {
    rowMeans(apply(stat, 1, function(s) { s > stat0 } ))
  }
}

LL.norm.var.equal.test <- function(x, y, z, permutations) {
  sd.z <- sd(z)
  par.x <- c(mean(x), sd.z)
  par.y <- c(mean(y), sd.z)
  stat0 <- likelyhood.test.stat(x, y, par.x, par.y, 'norm')
  stat <- apply(permutations, 1, function(z) { 
    x <- z[1:n]
    y <- z[(n+1):(2*n)]
    par.x <- c(mean(x), sd.z)
    par.y <- c(mean(y), sd.z)
    likelyhood.test.stat(x, y, par.x, par.y, 'norm') 
  })
  
  if (randomization) {
    phi(stat0, stat)
  } else {
    mean(stat > stat0)
  }
}

LL.norm.mean.equal.test <- function(x, y, z, permutations) {
  mean.z <- mean(z)
  par.x <- c(mean.z, sd.center(x, mean.z))
  par.y <- c(mean.z, sd.center(y, mean.z))
  stat0 <- likelyhood.test.stat(x, y, par.x, par.y, 'norm')
  stat <- apply(permutations, 1, function(z) { 
    x <- z[1:n]
    y <- z[(n+1):(2*n)]
    par.x <- c(mean.z, sd.center(x, mean.z))
    par.y <- c(mean.z, sd.center(y, mean.z))
    likelyhood.test.stat(x, y, par.x, par.y, 'norm') 
  })
  
  if (randomization) {
    phi(stat0, stat)
  } else {
    mean(stat > stat0)
  }
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
  
  stat <- apply(permutations, 1, function(z) { 
    x <- z[1:n]
    y <- z[(n+1):(2*n)]
    par.x <- find.distribution.par(x, distribution)
    par.y <- find.distribution.par(y, distribution)
    likelyhood.test.stat(x, y, par.x, par.y, distribution) 
  })
  
  if (randomization) {
    phi(stat0, stat)
  } else {
    mean(stat > stat0)
  }
}
