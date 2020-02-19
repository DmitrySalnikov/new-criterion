source('../funcs.R')

# params

alpha <- 0.05
n <- 5
M <- 1000

ress <- '../../res/levy/'
if (!dir.exists(ress)) dir.create(ress)
tables <- '../../tables/levy/'
if (!dir.exists(tables)) dir.create(tables)

# modelling

par <- seq(0, 10, length.out = 5)
first <- par
second <- 1+2*par
res <- vector()
for(x in par) res <- rbind(res, Power(t(replicate(M, c(rlevy(n,first[1],second[1]), rlevy(n,x,1+2*x)))), exact = TRUE))
res

# store

rownames(res) <- par
rname <- paste0('mean2varN',n,'M',M,'Exact')
saveRDS(res, paste0(ress, rname, '.RDS'))

tname <- rname
prefix <- 'L'
c2 <- c(paste0(prefix, '(', first, ', ', second, ')'))
c1 <- c(c2[1], rep('', 4))
res <- cbind(c1, c2, round(res,3))
res
MakeTable()