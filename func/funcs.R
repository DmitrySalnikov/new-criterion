library(rootSolve)

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

mean.var <- function(s) {
  res0 <- c(mean(s, trim=0.24), IQR(s))

  res <- multiroot(
    f = function(x) {
      F1 <- 0
      F2 <- -length(s)/2
      for (i in s) {
        F1 <- F1 + (i - x[1]) / (x[2]^2 + (i - x[1])^2)
        F2 <- F2 + ((i - x[1])^2) / (x[2]^2 + (i - x[1])^2)
      }
      c(F1 = F1, F2 = F2)
    }, start = res0)$root
  res[2] <- abs(res[2])

  if (res[2] < min(s-res[1]) || res[2] > max(s-res[1]) || res[2] > sd(s)*10) {
    res0
  } else {
    res
  }
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

K <- function(Z, A) {
  X <- Z[1:n]
  Y <- Z[(n+1):(2*n)]
  mX <- mean(X)
  mY <- mean(Y)
  vX <- sum( ( X-mX )**2 ) / n
  vY <- sum( ( Y-mY )**2 ) / n
  X.mean.var <- mean.var(X)
  Y.mean.var <- mean.var(Y)

  tmp <- vector()
  for (y in Y) {
    tmp <- c(tmp, abs(y - X))
  }
  tmpA <- tmp / A

  cc3 = 0
  for (y in Y) {
    cc3 <- cc3 + log( 1 + abs( X / Y.mean.var[2] - y / X.mean.var[2] )**2 )
  }

  c(K1 = sum(tmp),
    K2 = (mX - mY)**2,
    L1 = sum(log(1 + tmp)),
    L1C = sum(log(1 + tmpA)),
    L2 = sum(log(1 + tmp**2)),
    L2C = sum(log(1 + tmpA**2)),
    # L2new = sum(G(X - mY)) + sum(G(Y - mX)),
    # L2Cnew = sum(G( (n-1) * (X - mY) / sum((X - mX)**2) ))  + sum(G( (n-1) * (Y - mX) / sum((Y - mY)**2) )),
    # L2Cnew.med = sum(G( (n-1) * (X - Ymed) / sum((X - Xmed)**2) ))  + sum(G( (n-1) * (Y - Xmed) / sum((Y - Ymed)**2) )),
    #L0.5 = sum(log(1 + tmp**.5)),
    #L0.5C = sum(log(1 + tmpA**.5)),
    #K10 = sum(log(tmp)),
    T1 = T1(X, Y, X.mean.var[1], Y.mean.var[1]),
    new_norm_criterion = ( vX + (mX - mY)**2 ) / vY + ( vY + (mX - mY)**2 ) / vX,
    #new_cauchy_criterion = sum(log( 1 + abs(X - Y.mean.var[1]) )) + sum(log( 1 + abs(Y - X.mean.var[1]) )),
    #new_cauchy_criterion2 = sum(log( 1 + (abs(X - Y.mean.var[1]))**2 )) + sum(log( 1 + (abs(Y - X.mean.var[1]))**2 )),
    new_cauchy_criterion_n = sum(log( 1 + abs(X - Y.mean.var[1]) / Y.mean.var[2] )) + sum(log( 1 + abs(Y - X.mean.var[1]) / X.mean.var[2] )),
    new_cauchy_criterion2_n = -(sum(log( 1 + (abs(X - X.mean.var[1]) / X.mean.var[2] )**2 )) + sum(log( 1 + (abs(Y - Y.mean.var[1]) / Y.mean.var[2])**2 )) + n * ( log(X.mean.var[2]) + log(Y.mean.var[2]) )),
    new_cauchy_criterion3 = sum(cc3)
  )
}

Power <- function(Zd, exact = FALSE) {
  print("step")
  rowMeans(apply(Zd,1,function(Z) {
    A <- 0
    for (i in 1:(2*n-1))
      for (j in (i+1):(2*n))
        A <- A + abs(Z[i]-Z[j])
    A <- A/(n*(2*n-1))

    stat <- K(Z, A)

    perm <- if (exact) exact_perm(Z[1:5], Z[6:10]) else t(replicate(D,sample(Z)))
    stat <- rbind(stat, t(apply(perm,1,function(Zp) { K(Zp, A) })))

    res <- c(rowMeans(apply(stat[-1,], 1, function(x) x > stat[1,])),
      t.test      = t.test(Z[1:n], Z[(n+1):(2*n)], var.equal = FALSE)$p.value,
      wilcox.test = wilcox.test(Z[1:n], Z[(n+1):(2*n)])$p.value,
      ks.test     = ks.test(Z[1:n], Z[(n+1):(2*n)])$p.value,
      var.test    = var.test(Z[1:n], Z[(n+1):(2*n)])$p.value
    ) < alpha
  }))
}

MakeTable <- function(idx1 = vector(), with_F1 = FALSE) {
  # if (!length(idx2))
  #   write.table(data.frame("$F_1$", "$F_2$",
  #                          t(paste0('$\\K_{', substr(colnames(res)[idx1],2,10), '}$'))
  #   ), paste0(tables, tname), quote = F, sep = ' & ', row.names = F,
  #   eol = ' \\\\\ \\hline\n', col.names = F)
  # else
  #   write.table(data.frame("$F_1$", "$F_2$",
  #                          t(paste0('$\\K_{', substr(colnames(res)[idx1],2,10), '}$')),
  #                          t(colnames(res)[idx2])
  #   ), paste0(tables, tname), quote = F, sep = ' & ', row.names = F,
  #   eol = ' \\\\\ \\hline\n', col.names = F)
  if (length(idx1)) res <- res[,c(1,2,idx1)]
  if (!with_F1) res <- res[,-1]

  cat('% ', file = paste0(tables, tname, '.tex'))
  write.table(res, paste0(tables, tname, '.tex'),
              quote = F, sep = ' & ', row.names = F, eol = ' \\\\\n',
              col.names = T, append = TRUE)
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
