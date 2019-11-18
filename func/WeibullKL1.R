source('/home/d/Study/publication2019/func/funcs.R')
library('stats')
# params
alpha <- 0.05
M <- 1024
D <- 900

ress <- '/home/d/Study/publication2019/res/weibull/kl1/'
tables <- '/home/d/Study/publication2019/tables/weibull/kl1/'

par <- seq(2, 18, length.out = 5)
prefix <- 'W'
first = par
second <- 1
c2 <- paste0('$',prefix,'(',first,', ',second,')$')
c1 <- c(c2[1], rep('',4))
# n=5 exact
{
  n <- 5
  N = 2*n
  res <- vector()
  for(x in par) {
    res <- rbind(res, PowerAll(t(replicate(M, c(rweibull(n,par[1],second), rweibull(n,x,second)))), exact = TRUE))
  }
  rownames(res) <- par
  rname <- paste0('N',n,'M',M,'Exact')
  saveRDS(res, paste0(ress, rname, '.RDS'))
  
  res <- round(res, 3)
  res <- cbind(c2, res)
  res <- cbind(c1, res)
  print(res)
  
  tname <- rname
  MakeTable(3:12,13:16)
  
  tname = paste0(rname,'_OldTests')
  MakeTable(c(3,5,12),13:15)
  
  tname = paste0(rname,'_NewTests')
  MakeTable(6:11)
}
# var n=20
par <- seq(2, 10, length.out = 5)
first = par
second <- 1
c2 <- paste0('$',prefix,'(',first,', ',second,')$')
c1 <- c(c2[1], rep('',4))
{
  n <- 20
  N = 2*n
  res <- vector()
  for(x in par) {
    res <- rbind(res, PowerAll(t(replicate(M, c(rweibull(n,par[1],second), rweibull(n,x,second))))))
  }
  rownames(res) <- par
  rname <- paste0('N',n,'M',M,'D',D)
  saveRDS(res, paste0(ress, rname, '.RDS'))
  
  res <- round(res, 3)
  res <- cbind(c2, res)
  res <- cbind(c1, res)
  print(res)
  
  tname <- rname
  MakeTable(3:12,13:16)
  
  tname = paste0(rname,'_OldTests')
  MakeTable(c(3,5,12),13:15)
  
  tname = paste0(rname,'_NewTests')
  MakeTable(6:11)
}
