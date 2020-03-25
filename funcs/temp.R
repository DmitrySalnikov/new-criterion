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
    
    ######################################################################################################
    
    log.likelyhood <- function(par, x, distribution) {
      switch (distribution,
              norm = {
                -sum(dnorm(x, mean = par[1], sd = par[2], log = TRUE))
              }, cauchy = {
                -sum(dcauchy(x, location = par[1], scale = par[2], log = TRUE))
              }, levy = {
                -sum(dlevy(x, m = par[1], s = par[2], log = TRUE))
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
    
    K <- function(X, Y, only_positive) {
      tmp <- vector()
      for (y in Y) {
        tmp <- c(tmp, abs(y - X))
      }
      
      res <- c(
        L05 = sum(log(1 + tmp**.5)),
        L1 = sum(log(1 + tmp)),
        L2 = sum(log(1 + tmp**2)),
        LLnorm = likelyhood.test.stat(X, Y, 'norm'),
        LLcauchy = likelyhood.test.stat(X, Y, 'cauchy'),
        LLlevy = likelyhood.test.stat(X, Y, 'levy')
      )
      if (only_positive) {
        res <- c(res, 
          LLlogcauchy = likelyhood.test.stat(X, Y, 'logcauchy'),
        )
      }
      
      res
    }
    
    M <- 200
    D <- 800
    alpha <- 0.05
    n <- 50
    only_positive = FALSE
    
    rowMeans(apply(t(replicate(M, c(rnorm(n,0,1), rnorm(n,0.5,1)))), 1, function(Z) {
      X <- Z[1:n]
      Y <- Z[(n+1):(2*n)]
      perm <- t(replicate(D,sample(Z)))
      
      stat <- K(X, Y, only_positive)
      stat <- rbind(stat, t(apply(perm, 1, function(Zp) { K(Zp[1:n], Zp[(n+1):(2*n)], only_positive) })))
      
      c(rowMeans(apply(stat[-1,], 1, function(x) x > stat[1,])),
        wilcox.test = wilcox.test(X, Y)$p.value,
        ks.test     = ks.test(X, Y)$p.value,
        t.test      = t.test(X, Y)$p.value
      ) < alpha
    }))
