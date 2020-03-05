source('/home/d/1/new_criteria/func/funcs.R')

# params

alpha <- 0.05
n <- 50
M <- 1000
D <- 800

ress <- '../../res/logcauchy/'
if (!dir.exists(ress)) dir.create(ress)
tables <- '../../tables/logcauchy/'
if (!dir.exists(tables)) dir.create(tables)

# modelling

par <- seq(0, 1, length.out = 5)
first <- par
second <- 1+par
res <- vector()
for(x in par) res <- rbind(res, Power(t(replicate(M, c(rlogcauchy(n,first[1],second[1]), rlogcauchy(n,x,1+x)))), exact = FALSE, logcauchy = TRUE))
res

# store

rownames(res) <- par
rname <- paste0('mean1varN',n,'M',M,'D',D)
saveRDS(res, paste0(ress, rname, '.RDS'))

tname <- rname
prefix <- 'LC'
c2 <- c(paste0(prefix, '(', first, ', ', second, ')'))
c1 <- c(c2[1], rep('', 4))
res <- cbind(c1, c2, round(res,3))
res
MakeTable()