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

# norm var n=5 exact
par <- seq(1, 5, length.out = 5)
first <- 0
second <- par
res <- vector()
for(x in par) {
  res <- rbind(res, Power(t(replicate(M, c(rnorm(n,0,1), rnorm(n,0,x)))), exact = TRUE))
}
res
rownames(res) <- par
rname <- paste0('varExactN',n,'M',M)
saveRDS(res, paste0(ress, rname, '.RDS'))

tname <- rname
prefix <- 'N'
c1 <- c(paste0(prefix, '(', first, ', ', second[1], ')'), rep('', 4))
c2 <- c(paste0(prefix, '(', first, ', ', second, ')'))
res <- cbind(c1, c2, round(res,3))
res
MakeTable(3:6,12:17)