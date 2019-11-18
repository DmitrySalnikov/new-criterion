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

K <- function(Z, A) {
  X1 <- Z[1:n]
  X2 <- Z[(n+1):(2*n)]
  tmp <- vector()
  for (i in Z[-(1:n)]) 
    tmp <- c(tmp, abs(i - Z[1:n]))
  tmpA <- tmp/A
  c(K1 = (mean(X1)-mean(X2))**2, 
    K4 = (median(X1)-median(X2))**2, 
    K6 = sum(tmp),
    K7 = sum(log(1+tmp)),
    K71 = sum(log(1+tmpA)),
    K8 = sum(log(1+tmp**2)),
    K81 = sum(log(1+tmpA**2)),
    K9 = sum(log(1+tmp**.5)),
    K91 = sum(log(1+tmpA**.5)),
    K10 = sum(log(tmp))
  )
}

Kvar <- function(Z, A) {
  X1 <- Z[1:n]
  X2 <- Z[(n+1):(2*n)]
  tmp <- vector()
  for (i in Z[-(1:n)]) 
    tmp <- c(tmp, abs(i - Z[1:n]))
  tmpA <- tmp/A
  c(K6 = sum(tmp),
    K7 = sum(log(1+tmp)),
    K71 = sum(log(1+tmpA)),
    K8 = sum(log(1+tmp**2)),
    K81 = sum(log(1+tmpA**2)),
    K9 = sum(log(1+tmp**.5)),
    K91 = sum(log(1+tmpA**.5)),
    K10 = sum(log(tmp))
  )
}

Power <- function(Zd, exact = FALSE) {
  print("step")
  rowMeans(apply(Zd,1,function(Z) {
    A <- 0
    for (i in 1:(N-1)) 
      for (j in (i+1):N)
        A <- A + abs(Z[i]-Z[j])
      A <- A/(n*(2*n-1))
      
      stat <- K(Z, A)
      perm <- if (exact) exact_perm(Z[1:5], Z[6:10]) else t(replicate(D,sample(Z)))
      
      stat <- rbind(stat, t(apply(perm,1,function(Zp) { K(Zp, A) })))
      
      c(rowMeans(apply(stat[-1,], 1, function(x) x > stat[1,])), 
        t.test      = t.test(Z[1:n], Z[(n+1):(2*n)])$p.value, 
        wilcox.test = wilcox.test(Z[1:n], Z[(n+1):(2*n)])$p.value,
        ks.test     = ks.test(Z[1:n], Z[(n+1):(2*n)])$p.value
      ) < alpha
  }))
}

PowerVar <- function(Zd, exact = FALSE) {
  print("step")
  rowMeans(apply(Zd,1,function(Z) {
    A <- 0
    for (i in 1:(N-1)) 
      for (j in (i+1):N)
        A <- A + abs(Z[i]-Z[j])
      A <- A/(n*(2*n-1))
      
      stat <- Kvar(Z, A)
      perm <- if (exact) exact_perm(Z[1:5], Z[6:10]) else t(replicate(D,sample(Z)))
      
      stat <- rbind(stat, t(apply(perm,1,function(Zp) { Kvar(Zp, A) })))
      
      c(rowMeans(apply(stat[-1,], 1, function(x) x > stat[1,])), 
        ks.test = ks.test(Z[1:n], Z[(n+1):(2*n)])$p.value,
        var.test = var.test(Z[1:n], Z[(n+1):(2*n)])$p.value
      ) < alpha
  }))
}

MakeTable <- function(idx1,idx2 = vector()) {
  if (!length(idx2))
    write.table(data.frame("$F_1$", "$F_2$", 
                           t(paste0('$\\K_{', substr(colnames(res)[idx1],2,10), '}$'))
    ), paste0(tables, tname), quote = F, sep = ' & ', row.names = F, 
    eol = ' \\\\\ \\hline\n', col.names = F)
  else
    write.table(data.frame("$F_1$", "$F_2$", 
                           t(paste0('$\\K_{', substr(colnames(res)[idx1],2,10), '}$')),
                           t(colnames(res)[idx2])
    ), paste0(tables, tname), quote = F, sep = ' & ', row.names = F, 
    eol = ' \\\\\ \\hline\n', col.names = F)
  write.table(res[,c(1:2,idx1,idx2)], paste0(tables, tname), 
              quote = F, sep = ' & ', row.names = F, eol = ' \\\\\n', 
              col.names = F, append = TRUE)
  write('\\hline', paste0(tables, tname), append = T)
}

rNormCauchy <- function(n,perc,mu,sigma) {
  rb <- rbinom(n = n,size = 1, prob = perc)
  nc <- sum(rb)
  res <- vector(length = n)
  res[rb==1] <- rcauchy(nc,mu,sigma)
  res[rb==0] <- rnorm(n-nc,mu,sigma)
  res
}

# params

alpha <- 0.05
M <- 1024
D <- 900
perc <- 0.1

ress <- '/home/d/Study/publication2019/res/norm_10cauchy/mean/'
tables <- '/home/d/Study/publication2019/tables/norm_10cauchy/mean/'

# n=5 exact
{
  n <- 5
  N = 2*n
  par <- seq(0, 2, length.out = 5)
  res <- vector()
  for(x in par) {
    res <- rbind(res, Power(t(replicate(M, c(rNormCauchy(n,perc,0,1), rNormCauchy(n,perc,x,1)))), exact = TRUE))
  }
  res
  rownames(res) <- par
  rname <- paste0('N',n,'M',M,'Exact')
  saveRDS(res, paste0(ress, rname, '.RDS'))
  
  c1 <- c("$N10C(0, 1)$", '', '', '', '')
  c2 <- c("$N10C(0, 1)$", "$N10C(0.5, 1)$", "$N10C(1, 1)$", "$N10C(1.5, 1)$", "$N10C(2, 1)$")
  res <- round(res, 3)
  res <- cbind(c2, res)
  res <- cbind(c1, res)
  res
  
  tname <- rname
  MakeTable(3:12,13:15)
  
  tname = paste0(rname,'_OldTests')
  MakeTable(c(3,5,12),13:15)
  
  tname = paste0(rname,'_NewTests')
  MakeTable(6:11)
}
# var n=20
{
  n <- 20
  N = 2*n
  par <- seq(0, 2, length.out = 5)
  res <- vector()
  for(x in par) {
    res <- rbind(res, Power(t(replicate(M, c(rNormCauchy(n,perc,0,1), rNormCauchy(n,perc,x,1))))))
  }
  res
  rownames(res) <- par
  rname <- paste0('N',n,'M',M,'D',D)
  saveRDS(res, paste0(ress, rname, '.RDS'))
  
  c1 <- c("$N10C(0, 1)$", '', '', '', '')
  c2 <- c("$N10C(0, 1)$", "$N10C(0.5, 1)$", "$N10C(1, 1)$", "$N10C(1.5, 1)$", "$N10C(2, 1)$")
  res <- round(res, 3)
  res <- cbind(c2, res)
  res <- cbind(c1, res)
  res
  
  tname <- rname
  MakeTable(3:12,13:15)
  
  tname = paste0(rname,'_OldTests')
  MakeTable(c(3,5,12),13:15)
  
  tname = paste0(rname,'_NewTests')
  MakeTable(6:11)
}
