source('/home/d/Study/new_criteria/func/funcs.R')

# params

alpha <- 0.05
n <- 5
M <- 1000
D <- 800

ress <- '/home/d/Study/new_criteria/res/norm/'
if (!dir.exists(ress)) dir.create(ress)
tables <- '/home/d/Study/new_criteria/tables/norm/'
if (!dir.exists(tables)) dir.create(tables)

# n=5 exact

par <- seq(0, 2, length.out = 5)
first <- par
second <- 1+par
res <- vector()
for(x in par) {
  res <- rbind(res, Power(t(replicate(M, c(rnorm(n,0,1), rnorm(n,x,1+x)))), exact = TRUE))
}
res
rownames(res) <- par
rname <- paste0('mean2VarExactN',n,'M',M)
saveRDS(res, paste0(ress, rname, '.RDS'))
  
prefix <- 'N'
c2 <- paste0('$',prefix,'(',first,', ',second,')$')
c1 <- c(c2[1], rep('',4))
res <- cbind(c1, c2, round(res,3))
res
MakeTable(3:6,12:17)