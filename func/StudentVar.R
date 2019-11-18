source('/home/d/Study/publication2019/func/funcs.R')
# params
alpha <- 0.05
M <- 1024
D <- 900

ress <- '/home/d/Study/publication2019/res/student/var/'
tables <- '/home/d/Study/publication2019/tables/student/var/'

par <- seq(1, 25, length.out = 5)
prefix <- 'St'
first <- par
second <- 0
c2 <- paste0('$',prefix,'(',first,', ',second,')$')
c1 <- c(c2[1], rep('',4))
# n=5 exact
{
  n <- 5
  N = 2*n
  res <- vector()
  for(x in par) {
    res <- rbind(res, PowerVar(t(replicate(M, c(rt(n,df = par[1],ncp = second), rt(n,x,second)))), exact = TRUE))
  }
  rownames(res) <- par
  rname <- paste0('N',n,'M',M,'Exact')
  saveRDS(res, paste0(ress, rname, '.RDS'))
  
  res <- round(res, 3)
  res <- cbind(c2, res)
  res <- cbind(c1, res)
  print(res)
  
  tname <- rname
  MakeTable(3:10,11:12)
  
  tname = paste0(rname,'_OldTests')
  MakeTable(c(5,7,9),11:12)
  
  tname = paste0(rname,'_NewTests')
  MakeTable(c(3,4,6,8,10))
}
# var n=20
par <- seq(1, 13, length.out = 5)
first <- par
second <- 0
c2 <- paste0('$',prefix,'(',first,', ',second,')$')
c1 <- c(c2[1], rep('',4))
{
  n <- 20
  N = 2*n
  res <- vector()
  for(x in par) {
    res <- rbind(res, PowerVar(t(replicate(M, c(rt(n,par[1],second), rt(n,x,second))))))
  }
  rownames(res) <- par
  rname <- paste0('N',n,'M',M,'D',D)
  saveRDS(res, paste0(ress, rname, '.RDS'))
  
  res <- round(res, 3)
  res <- cbind(c2, res)
  res <- cbind(c1, res)
  print(res)
  
  tname <- rname
  MakeTable(3:10,11:12)
  
  tname = paste0(rname,'_OldTests')
  MakeTable(c(5,7,9),11:12)
  
  tname = paste0(rname,'_NewTests')
  MakeTable(c(3,4,6,8,10))
}
