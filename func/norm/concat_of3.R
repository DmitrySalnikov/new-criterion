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

K <- function(Z) {
  X1 <- Z[1:n]
  X2 <- Z[(n+1):(2*n)]
  mX1 <- mean(X1)
  mX2 <- mean(X2)
  vX1 <- sum((X1-mX1)**2)/n
  vX2 <- sum((X2-mX2)**2)/n
  
  c(NC = (vX1+(mX1-mX2)**2)/vX2 + (vX2+(mX1-mX2)**2)/vX1)
}

Power <- function(Zd, exact = FALSE) {
  print("step")
  rowMeans(apply(Zd,1,function(Z) {
    stat <- K(Z)
    
    perm <- if (exact) exact_perm(Z[1:5], Z[6:10]) else t(replicate(D,sample(Z)))
    stat <- c(stat, t(apply(perm,1,function(Zp) { K(Zp) })))
    
    res <- c(NC = mean(stat[-1] > stat[1]), 
             t.test      = t.test(Z[1:n], Z[(n+1):(2*n)],var.equal = FALSE)$p.value, 
             var.test    = var.test(Z[1:n], Z[(n+1):(2*n)])$p.value
    ) < alpha
    if (res['t.test']) {
      res <- c(res, concat = TRUE)
    } else if (res['var.test']) {
      res <- c(res, concat = TRUE)
    } else {
      res <- c(res, concat = res['NC'])
    }
    res
  }))
}

alpha <- 0.05
n <- 5
M <- 1000
D <- 800

Power(t(replicate(M, c(rnorm(n,0,1), rnorm(n,0,1)))), exact = TRUE)
