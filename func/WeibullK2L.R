source('/home/d/Study/publication2019/func/funcs.R')
library('stats')
# params
alpha <- 0.05
M <- 1024
D <- 900

ress <- '/home/d/Study/publication2019/res/weibull/k2l/'
tables <- '/home/d/Study/publication2019/tables/weibull/k2l/'

par <- seq(1, 3, length.out = 5)
prefix <- 'W'
first = 2
second <- par
c2 <- paste0('$',prefix,'(',first,', ',second,')$')
c1 <- c(c2[1], rep('',4))
# n=5 exact
{
  n <- 5
  N = 2*n
  res <- vector()
  for(x in par) {
    res <- rbind(res, PowerAll(t(replicate(M, c(rweibull(n,first,par[1]), rweibull(n,first,x)))), exact = TRUE))
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
par <- seq(1, 2, length.out = 5)
first = 2
second <- par
c2 <- paste0('$',prefix,'(',first,', ',second,')$')
c1 <- c(c2[1], rep('',4))
{
  n <- 20
  N = 2*n
  res <- vector()
  for(x in par) {
    res <- rbind(res, PowerAll(t(replicate(M, c(rweibull(n,first,par[1]), rweibull(n,first,x))))))
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
