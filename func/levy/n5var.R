source('../funcs.R')

# params

alpha <- 0.05
n <- 5
M <- 1000

ress <- '/home/d/1/new_criteria/res/levy/'
if (!dir.exists(ress)) dir.create(ress)
tables <- '/home/d/1/new_criteria/tables/levy/'
if (!dir.exists(tables)) dir.create(tables)

# modelling

par <- seq(1, 25, length.out = 5)
first <- 0
second <- par
res <- vector()
for(x in par) res <- rbind(res, Power(t(replicate(M, c(rlevy(n,first,second[1]), rlevy(n,first,x)))), exact = TRUE))
res

# store

rownames(res) <- par
rname <- paste0('varN',n,'M',M,'Exact')
saveRDS(res, paste0(ress, rname, '.RDS'))

tname <- rname
prefix <- 'L'
c2 <- c(paste0(prefix, '(', first, ', ', second, ')'))
c1 <- c(c2[1], rep('', 4))
res <- cbind(c1, c2, round(res,3))
res
MakeTable()
