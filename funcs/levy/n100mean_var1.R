source('/home/d/Study/new_criteria/func/funcs.R')

# params

alpha <- 0.05
n <- 100
M <- 1000
D <- 800

ress <- '/home/d/Study/new_criteria/res/levy/'
if (!dir.exists(ress)) dir.create(ress)
tables <- '/home/d/Study/new_criteria/tables/levy/'
if (!dir.exists(tables)) dir.create(tables)

#####################################################################################

par <- seq(0, 4, length.out = 5)
first <- par
second <- 1+par
res <- vector()
for(x in par) res <- rbind(res, Power(t(replicate(M, c(rlevy(n,first[1],second[1]), rlevy(n,x,1+x)))), exact = TRUE))
res

rownames(res) <- par
rname <- paste0('meanVarExactN',n,'M',M)
saveRDS(res, paste0(ress, rname, '.RDS'))

tname <- rname
prefix <- 'L'
c1 <- c(paste0(prefix, '(', first[1], ', ', second[1], ')'), rep('', 4))
c2 <- c(paste0(prefix, '(', first, ', ', second, ')'))
res <- cbind(c1, c2, round(res,3))
res
MakeTable(3:6,12:17)