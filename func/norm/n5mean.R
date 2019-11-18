source('/home/d/1/new_criteria/func/funcs.R')

# params

alpha <- 0.05
n <- 5
M <- 1000

ress <- '/home/d/1/new_criteria/res/norm/'
if (!dir.exists(ress)) dir.create(ress)
tables <- '/home/d/1/new_criteria/tables/norm/'
if (!dir.exists(tables)) dir.create(tables)

# modelling

par <- seq(0, 2, length.out = 5)
first <- par
second <- 1
res <- vector()
for(x in par) res <- rbind(res, Power(t(replicate(M, c(rnorm(n,first[1],second), rnorm(n,x,second)))), exact = TRUE))
res

# store

rownames(res) <- par
rname <- paste0('meanN',n,'M',M,'Exact')
saveRDS(res, paste0(ress, rname, '.RDS'))

tname <- rname
prefix <- 'N'
c2 <- c(paste0(prefix, '(', first, ', ', second, ')'))
c1 <- c(c2[1], rep('', 4))
res <- cbind(c1, c2, round(res,3))
res
MakeTable()
