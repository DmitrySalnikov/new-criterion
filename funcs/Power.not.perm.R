path = '/home/d/1/new_criteria'
source(paste(path, 'funcs', 'static.R', sep = '/'))

Power.cauchy.not.perm <- function(type, par2, par1 = c(0, 1), n = 50, M = 1000, alpha = 0.05, prefix = 'not_perm,', K = 1000000) {
  start.time <- Sys.time()

  details <- paste0(prefix, 'par2=(', par2[1], ',', par2[2], '),n=', n, ',M=', M, ',K=', log(K,10))

  res.path <- create.folder(c(path, 'res', 'cauchy', type))
  res.name <- paste0(res.path, '/', details, '.RDS')

  if (file.exists(res.name)) {
    return()
  }

  print(paste0('cauchy, ', details, ', ', Sys.time() - start.time))

  set.seed(500)
  X.set <- rcauchy(n * M, par1[1], par1[2])
  Y.set <- rcauchy(n * M, par2[1], par2[2])
  dim(X.set) <- c(M, n)
  dim(Y.set) <- c(M, n)

  L2.thresholds <- apply(
    sapply(1:K, function(i) {
      if (i %% 10000 == 0) {
        print(paste0(i, ', ', Sys.time() - start.time))
      }
      x <- rcauchy(n, par1[1], par1[2])
      y <- rcauchy(n, par1[1], par1[2])
      L2 <- L2.test.stat(x, y)
      c(
        L2,
        L2.modified.test.stat(x, y, n, n, L2)
      )
    }
    ), MARGIN = 1, function(row) { quantile(row, 0.95) } )

  mu_x <- if (par2[1] == 0) 1 else par2[1]
  LL.threshold <- quantile(sapply(1:K, function(i) {
    LL.cauchy.test.stat(rcauchy(n, par1[1], par1[2]), mu_x)
  }), 0.95)

  res <- rowMeans(sapply(1:M, function(i) {
    if (i %% 1000 == 0) {
      print(paste0(i, ', ', Sys.time() - start.time))
    }
    X <- X.set[i, ]
    Y <- Y.set[i, ]

    L2.stat <- L2.test.stat(X, Y)
    L2.modified.stat <- L2.modified.test.stat(X, Y, n, n, L2.stat)
    LL.cauchy.stat <- LL.cauchy.test.stat(Y, mu_x)

    c(
      L2.not.perm          = L2.stat > L2.thresholds[1],
      L2.modified.not.perm = L2.modified.stat > L2.thresholds[2],
      LL.cauchy.not.perm   = LL.cauchy.stat > LL.threshold,
      c(
        wilcox.test = wilcox.test(X, Y)$p.value#,
        #ks.test     = ks.test(X, Y)$p.value,
        #t.test      = t.test(X, Y)$p.value,
        #var.test    = var.test(X, Y)$p.value
      ) < alpha
    )
  }))

  print(res)
  print(c(L2.thresholds, LL.threshold))

  saveRDS(list(c(L2.thresholds, LL.threshold), res), res.name)
}

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

LL.cauchy.test.stat <- function(x, mu.x) {
  2*sum(log( (x^2+1) / ((x-mu.x)^2+1) ))
}

find.cauchy.par <- function(z) {
  lower <- c(-Inf, 1e-6)
  upper <- c(Inf, Inf)
  par1.estim <- mean(z, trim = 0.24)
  par2.estim <- IQR(z) / 2

  par.estim <- c(par1.estim, par2.estim)

  cauchy.log.likelyhood <- function(par, z) {
    -sum(dcauchy(z, par[1], par[2], log = TRUE))
  }

  optim(par.estim, cauchy.log.likelyhood, method = "L-BFGS-B", lower = lower, upper = upper, z = z)$par
}

power.250 <- rbind(c(1,2,3,4,7,10), c(0.065, 0.115, 0.212, 0.340, 0.825, 0.988))
power.1000 <- rbind(c(1,2,3,4,7,10), c(0.06, 0.119, 0.209, 0.346, 0.845, 0.989))
power.10000 <- rbind(c(1,2,3,4,7,10), c(0.06, 0.1146, 0.2065, 0.3404, 0.8241, 0.9895))

analytic.power.MNK <- function(h, alpha = 0.05) {
  SUM <- function(par) {
    h <- power.10000[1,]
    p <- qnorm(power.10000[2,], lower.tail = FALSE)
    sum((qnorm(1-alpha/2) - par[1]*h - par[2]*h^2 - p)^2)
  }
  par <- optim(c(0.1, 0.2), SUM, method = "L-BFGS-B")$par

  pnorm(qnorm(1-alpha/2) - par[1]*h - par[2]*h^2, lower.tail = FALSE)
}

round(analytic.power.MNK(c(1,2,3,4,7,10)), 3)

analytic.power.MNK2 <- function(h, alpha = 0.05) {
  SUM <- function(par) {
    h <- power.10000[1,]
    p <- qnorm(power.10000[2,], lower.tail = FALSE)
    sum((qnorm(1-alpha/2) - par*h - p)^2)
  }
  par <- optim(c(0.1), SUM, method = "L-BFGS-B")$par

  pnorm(qnorm(1-alpha/2) - par*h, lower.tail = FALSE)
}

round(analytic.power.MNK2(c(1,2,3,4,7,10)), 3)

analytic.power.MNK3 <- function(h, alpha = 0.05) {
  SUM <- function(par) {
    h <- power.10000[1,]
    p <- qnorm(power.10000[2,], lower.tail = FALSE)
    sum((qnorm(1-alpha/2) - par*h^2 - p)^2)
  }
  par <- optim(c(0.1), SUM, method = "L-BFGS-B")$par

  pnorm(qnorm(1-alpha/2) - par*h^2, lower.tail = FALSE)
}

round(analytic.power.MNK3(c(1,2,3,4,7,10)), 3)

analytic.power <- function(h, alpha = 0.05) {
  v <- (6/9*h^2)^2
  b <- sqrt(sqrt(72+v) - sqrt(72))
  k <- sqrt(8.2) * qnorm(1-alpha/2)^2
  q1 <- (sqrt(k)-b)/sqrt(sqrt(8.2))
  q2 <- (-sqrt(k)-b)/sqrt(sqrt(8.2))

  pnorm(q1, lower.tail = FALSE) + pnorm(q2)
  pnorm(qnorm(1-alpha/2) - sqrt(2/3)*h/2.03, lower.tail = F)
}

new.analytic.power <- function(h, alpha = 0.05) {
  additive <- h / sqrt(6*log(3))
  q1 <- qnorm(1-alpha/2) - additive
  q2 <- -qnorm(1-alpha/2) - additive

  pnorm(q1, lower.tail = FALSE) + pnorm(q2)
}

round(analytic.power(c(1,2,3,4,7,10)), 3)

rbind(power.1000,
      round(analytic.power(c(1,2,3,4,7,10)), 3))

round(new.analytic.power(c(1,2,3,4,7,10)), 3)