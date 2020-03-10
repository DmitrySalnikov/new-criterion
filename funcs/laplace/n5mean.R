source('/home/d/1/new_criteria/funcs/funcs.R')

distribution_name <- tail(strsplit(getwd(), '/')[[1]], 1)

# params

alpha <- 0.05
n <- 5
M <- 1000

res_path <- paste0(path, 'res/', distribution_name)
if (!dir.exists(ress)) dir.create(ress)
tables_path <- paste0(path, 'tables/', distribution_name)
if (!dir.exists(tables)) dir.create(tables)

# modelling

par <- seq(0, 3, length.out = 5)
first <- par
second <- 1
res <- vector()
for(x in par) res <- rbind(res, Power(t(replicate(M, c(rlaplace(n,first[1],second), rlaplace(n,x,second)))), exact = TRUE))
res

# store

rownames(res) <- par
rname <- paste0('meanN',n,'M',M,'Exact')
saveRDS(res, paste0(ress, rname, '.RDS'))

tname <- rname
prefix <- 'L'
c2 <- c(paste0(prefix, '(', first, ', ', second, ')'))
c1 <- c(c2[1], rep('', 4))
res <- cbind(c1, c2, round(res,3))
res
MakeTable()
