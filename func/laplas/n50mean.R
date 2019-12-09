source('/home/d/1/new_criteria/func/funcs.R')
source('/home/d/1/new_criteria/func/laplas/rlaplas.R')

# params

alpha <- 0.05
n <- 50
M <- 1000
D <- 800

ress <- '/home/d/1/new_criteria/res/laplas/'
if (!dir.exists(ress)) dir.create(ress)
tables <- '/home/d/1/new_criteria/tables/laplas/'
if (!dir.exists(tables)) dir.create(tables)

# modelling

par <- seq(0, 1, length.out = 5)
first <- par
second <- 1
res <- vector()
for(x in par) res <- rbind(res, Power(t(replicate(M, c(rlaplas(n,first[1],second), rlaplas(n,x,second)))), exact = FALSE))
res

# store

rownames(res) <- par
rname <- paste0('meanN',n,'M',M,'D',D)
saveRDS(res, paste0(ress, rname, '.RDS'))

tname <- rname
prefix <- 'L'
c2 <- c(paste0(prefix, '(', first, ', ', second, ')'))
c1 <- c(c2[1], rep('', 4))
res <- cbind(c1, c2, round(res,3))
res
MakeTable()
