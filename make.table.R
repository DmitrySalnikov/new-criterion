source('/home/d/1/new_criteria/funcs/funcs.R')

short.names <- c(cauchy = 'C', norm = 'N', levy = 'Le', laplace = 'La', logcauchy = 'LC')
F2 <- function(distribution, par2) {
  paste0(short.names[distribution], '(', par2[1], ', ', par2[2], ')')
}

make.table.name <- function(distribution, n, M, D, randomization = TRUE, prefix = NULL) {
  exact <- n == 5
  if(exact) {
    D <- n.exact.perms(5, 5, 10)
  }
  details = paste0(path, '/tables/', prefix, distribution, ',n=', n, ',M=', M, ',D=', D)
  if(exact) {
    details <- paste0(details, ',exact')
  }
  if(randomization) {
    details = paste0(details, ',rand')
  }
  
  paste0(details, '.tex')
}

write.row <- function(distribution, type, par2, tests.numbers, n = 50, M = 1000, D = 800, col.names = FALSE, randomization = TRUE, prefix = NULL) {
  res <- c(F2(distribution, par2),
           round(read.res(distribution, type, par2, n, M, D, randomization = randomization, prefix = prefix)[tests.numbers], 3) * 100)
  table.name <- make.table.name(distribution, n, M, D, randomization, prefix = prefix)
  if (col.names) {
    cat('% ', file = table.name)
  }
  write.table(t(res), table.name, 
              quote = F, sep = ' & ', eol = ' \\\\\n',
              row.names = F, col.names = col.names, append = TRUE)
}

make.table <- function(distribution, par, tests.numbers, n = 50, M = 1000, D = 800, randomization = TRUE) {
  table.name <- make.table.name(distribution, n, M, D, randomization)
  write.row(distribution, 'mean', c(0, 1), tests.numbers, n, M, D, TRUE, randomization)
  write('\\hline', table.name, append = TRUE)
  for (x in par[-1]) {
    write.row(distribution, 'mean', c(x, 1), tests.numbers, n, M, D, randomization = randomization)
  }
  write('\\hline', table.name, append = TRUE)
  for (x in par[-1]) {
    write.row(distribution, 'var', c(0, 1 + 2 * x), tests.numbers, n, M,  D, randomization = randomization)
  }
  write('\\hline', table.name, append = TRUE)
  for (x in par[-1]) {
    write.row(distribution, 'mean1var', c(x, 1 + x), tests.numbers, n, M, D, randomization = randomization)
  }
  write('\\hline', table.name, append = TRUE)
  for (x in par[-1]) {
    write.row(distribution, 'mean2var', c(x, 1 + 2 * x), tests.numbers, n, M, D, randomization = randomization)
  }  
  write('\\hline', table.name, append = TRUE)
}

read.res('laplace', 'mean', cbind(1, 1), randomization = FALSE)
read.res('logcauchy', 'mean', cbind(1, 1))
read.res('norm', 'mean', cbind(0, 1), randomization = FALSE)
read.res('logcauchy', 'mean', cbind(0, 1), randomization = FALSE)
read.res('cauchy', 'mean', cbind(0, 1), randomization = TRUE)
read.res('levy', 'mean', cbind(0, 1), randomization = FALSE)
read.res('norm', 'mean', cbind(0, 1), randomization = TRUE)
read.res('cauchy', 'var', cbind(0, 1.8), randomization = FALSE, n = 200)

make.table('norm', seq(0, 1, length.out = 5), c(3, 5, 8:9, 12:16), randomization = FALSE)
make.table('cauchy', seq(0, 1, length.out = 5), c(3, 5, 8:9, 12:16), randomization = FALSE)
make.table('levy', seq(0, 1, length.out = 5), c(3, 5, 8:9, 12:16), randomization = FALSE)
make.table('laplace', seq(0, 1, length.out = 5), c(3, 6, 8:9, 12:16), randomization = FALSE)
make.table('logcauchy', seq(0, 1, length.out = 5), c(3, 5, 8:9, 12:13, 15, 17:18), randomization = FALSE)

make.table('norm', seq(0, 1, length.out = 5), c(3, 5, 7:9, 11:15), randomization = TRUE)
make.table('cauchy', seq(0, 1, length.out = 5), c(3, 5, 7:8, 11:15), randomization = TRUE)
make.table('levy', seq(0, 1, length.out = 5), c(3, 5, 7:8, 11:15), randomization = TRUE)
make.table('laplace', seq(0, 1, length.out = 5), c(3, 5, 7:8, 11:15), randomization = TRUE)
make.table('logcauchy', seq(0, 1, length.out = 5), c(3, 5, 7:8, 11:14, 16:17), randomization = TRUE)

make.table('cauchy', seq(0, 0.4, length.out = 5), c(3, 6, 5, 8:9, 12:16), randomization = FALSE, n = 200)

table.name <- make.table.name("significancy", n = 5, M = 1000, D = 0, randomization = FALSE)
cat('% ', file = table.name)
for (distr in DISTRIBUTIONS) {
  res <- rbind(c(paste("rand", F2(distr, c(0,1))), read.res(distr, 'mean', c(0, 1), 5, 1000, 0, randomization = TRUE)),
               c(F2(distr, c(0,1)), read.res(distr, 'mean', c(0, 1), 5, 1000, 0, randomization = FALSE)))
  cat('% ', file = table.name, append = TRUE)
  write.table(res[, 1 + c(0, 3, 5, 7:8, 11:13)], table.name, 
              quote = F, sep = ' & ', eol = ' \\\\\n',  row.names = F, col.names = T, append = TRUE)
  write('\\hline', table.name, append = TRUE)
}

res <- read.res('cauchy', 'mean', cbind(0, 1), randomization = FALSE, prefix = 'L2LLc', n = 1000)
write.row('cauchy', 'mean', cbind(0, 1), randomization = FALSE, prefix = 'L2LLc', n = 1000, tests.numbers = 1:4, col.names = TRUE)
for (par2 in seq(0.05, 0.2, length.out = 4)) {
  write.row('cauchy', 'mean', c(par2, 1), randomization = FALSE, prefix = 'L2LLc', n = 1000, tests.numbers = 1:4)
}
res1 <- read.res('cauchy', 'mean', c(1,1), n=50, randomization = FALSE)[c(5, 12, 15:16)]*100
res2 <- readRDS('res/cauchy/mean/not_perm,par2=(1,1),n=50,M=1000,K=10000.RDS')[1:3]*100
res
write.table(t(c(50, 'C(1,1)', read.res('cauchy', 'mean', c(1,1), n=50, randomization = FALSE)[c(5, 12, 15:16)]*100)), 'tables/L2LLc,M=1000,D=800.tex', 
            quote = F, sep = ' & ', eol = ' \\\\\n', row.names = F, col.names = F, append = FALSE)
write.table(t(c(200, 'C(0.5,1)', read.res('cauchy', 'mean', c(0.5,1), n=200, randomization = FALSE, prefix = 'L2LLc')[1:4]*100)), 'tables/L2LLc,M=1000,D=800.tex', 
            quote = F, sep = ' & ', eol = ' \\\\\n', row.names = F, col.names = F, append = TRUE)
write.table(t(c(1250, 'C(0.2,1)', read.res('cauchy', 'mean', c(0.2,1), n=1250, randomization = FALSE, prefix = 'L2LLc')[1:4]*100)), 'tables/L2LLc,M=1000,D=800.tex', 
            quote = F, sep = ' & ', eol = ' \\\\\n', row.names = F, col.names = F, append = TRUE)
write.table(t(c(5000, 'C(0.1,1)', read.res('cauchy', 'mean', c(0.1,1), n=5000, randomization = FALSE, prefix = 'L2LLc')[1:4]*100)), 'tables/L2LLc,M=1000,D=800.tex', 
            quote = F, sep = ' & ', eol = ' \\\\\n', row.names = F, col.names = F, append = TRUE)

nnn <- c(3, 5, 8, 9, 12:14, 15, 17, 18)
par <- c(1, 1)
res <- read.res('logcauchy', 'mean', par, randomization = FALSE)[nnn]*100
res
write.table(t(res), 'temp', 
            quote = F, sep = ' & ', eol = ' \\\\\n',
            row.names = F, col.names = F, append = FALSE)

res1 <- read.res('cauchy', 'mean', c(0,1), n=50, randomization = FALSE)[c(5, 12, 15:16)]*100
res2 <- readRDS('res/cauchy/mean/not_perm,par2=(0,1),n=50,M=1000,K=10000.RDS')[1:3]*100
res <- c(res1[1], res2[1:2], res1[2], res2[3], res1[3:4])
write.table(t(c(50, 'C(0,1)', res)), 'tables/not_perm,M=1000.tex', 
            quote = F, sep = ' & ', eol = ' \\\\\n', row.names = F, col.names = F, append = FALSE)
res1 <- read.res('cauchy', 'mean', c(1,1), n=50, randomization = FALSE)[c(5, 12, 15:16)]*100
res2 <- readRDS('res/cauchy/mean/not_perm,par2=(1,1),n=50,M=1000,K=10000.RDS')[1:3]*100
res <- c(res1[1], res2[1:2], res1[2], res2[3], res1[3:4])
write.table(t(c(50, 'C(1,1)', res)), 'tables/not_perm,M=1000.tex', 
            quote = F, sep = ' & ', eol = ' \\\\\n', row.names = F, col.names = F, append = TRUE)
res1 <- read.res('cauchy', 'mean', c(0.5,1), n=200, randomization = FALSE, prefix = 'L2LLc')[c(1:4)]*100
res2 <- readRDS('res/cauchy/mean/not_perm,par2=(0.5,1),n=200,M=1000,K=1600.RDS')[1:3]*100
res <- c(res1[1], res2[1:2], res1[2], res2[3], res1[3:4])
write.table(t(c(200, 'C(0.5,1)', res)), 'tables/not_perm,M=1000.tex', 
            quote = F, sep = ' & ', eol = ' \\\\\n', row.names = F, col.names = F, append = TRUE)

##################################################################################################################

h <- function(x, n) x/sqrt(n)

b <- function(n) 1/6/sqrt(n)

power.MNKab <- function(h, power, alpha = 0.05) {
  SUM <- function(par) {
    h <- power[1,]
    p <- qnorm(power[2,], lower.tail = FALSE)  
    sum((qnorm(1-alpha/2) - par[1]*h - par[2]*h^2 - p)^2)
  }
  par <- optim(c(0.1, 0.2), SUM, method = "L-BFGS-B", lower=c(0,0))$par
  
  list(pnorm(qnorm(1-alpha/2) - par[1]*h - par[2]*h^2, lower.tail = FALSE), par)
}

power.MNKa <- function(h, power, alpha = 0.05) {
  SUM <- function(par) {
    h <- power[1,]
    p <- qnorm(power[2,], lower.tail = FALSE)  
    sum((qnorm(1-alpha/2) - par*h - p)^2)
  }
  par <- optim(c(0.1), SUM, method = "L-BFGS-B", lower=0)$par
  
  list(pnorm(qnorm(1-alpha/2) - par*h, lower.tail = FALSE), par)
}

analytic.power <- function(h, n, alpha = 0.05) {
  a <-  1/sqrt(6)
  b <- 1/6/sqrt(n)
  list(pnorm(qnorm(1-alpha/2) - a*h - b*h^2, lower.tail = F), c(a,b))
}

# n=50
res <- {
  not_perm.n50 <- rbind(
    readRDS('res/cauchy/mean/not_perm,par2=(0.2828427,1),n=50,M=1000,K=6.RDS'),
    readRDS('res/cauchy/mean/not_perm,par2=(0.5656854,1),n=50,M=1000,K=6.RDS'),
    readRDS('res/cauchy/mean/not_perm,par2=(1.414214,1),n=50,M=1000,K=6.RDS'),
    readRDS('res/cauchy/mean/not_perm,par2=(1.697056,1),n=50,M=1000,K=6.RDS')
  )[,2:3]
  h <- c(2,4,10,12)
  MNKab <- round(power.MNKab(h, rbind(h, not_perm.n50[,1]))[[1]], 4)*100
  MNKa <- round(power.MNKa(h, rbind(h, not_perm.n50[,1]))[[1]], 4)*100
  n <- 50
  analytic <- round(analytic.power(h, n)[[1]], 4)*100
  not_perm.n50 <- not_perm.n50*100
  
  cbind(h, L2_n.p=not_perm.n50[,1], analytic, MNKab=MNKab, MNKa=MNKa, LLc_n.p=not_perm.n50[,2])
}
tname <- 'tables/not_perm,n=50,M=1000.tex'
cat('% ', file=tname)
write.table(res, tname, 
            quote = F, sep = ' & ', eol = ' \\\\\n', row.names = F, col.names = T, append = TRUE)
# h(c(2,4,10,12), 50)

# n=250
res <- {
  not_perm.n250 <- rbind(
    readRDS('res/cauchy/mean/not_perm,par2=(0.0632455532033676,1),n=250,M=1000,K=6.RDS'),
    readRDS('res/cauchy/mean/not_perm,par2=(0.126491106406735,1),n=250,M=1000,K=6.RDS'),
    readRDS('res/cauchy/mean/not_perm,par2=(0.189736659610103,1),n=250,M=1000,K=6.RDS'),
    readRDS('res/cauchy/mean/not_perm,par2=(0.25298221281347,1),n=250,M=1000,K=6.RDS'),
    readRDS('res/cauchy/mean/not_perm,par2=(0.442718872423573,1),n=250,M=1000,K=6.RDS'),
    readRDS('res/cauchy/mean/not_perm,par2=(0.632455532033676,1),n=250,M=1000,K=6.RDS')
  )[,2:3]
  h <- c(1,2,3,4,7,10)
  MNKab <- round(power.MNKab(h, rbind(h, not_perm.n250[,1]))[[1]], 4)*100
  MNKa <- round(power.MNKa(h, rbind(h, not_perm.n250[,1]))[[1]], 4)*100
  n <- 250
  analytic <- round(analytic.power(h, n)[[1]], 4)
  not_perm.n250 <- not_perm.n250*100
  
  cbind(h, L2_n.p=not_perm.n250[,1], analytic, MNKab=MNKab, MNKa=MNKa, LLc_n.p=not_perm.n250[,2])
}
tname <- 'tables/not_perm,n=250,M=1000.tex'
cat('% ', file=tname)
write.table(res, tname, 
            quote = F, sep = ' & ', eol = ' \\\\\n', row.names = F, col.names = T, append = TRUE)
# h(c(1,2,3,4,7,10), 250)

#n=1000, M=10000
res <- {
  not_perm.M10000 <- rbind(
    readRDS('res/cauchy/mean/not_perm,par2=(0.0632455532033676,1),n=1000,M=10000,K=6.RDS')[[2]],
    readRDS('res/cauchy/mean/not_perm,par2=(0.0948683298050514,1),n=1000,M=10000,K=6.RDS')[[2]],
    readRDS('res/cauchy/mean/not_perm,par2=(0.126491106406735,1),n=1000,M=10000,K=6.RDS')[[2]],
    readRDS('res/cauchy/mean/not_perm,par2=(0.221359436211787,1),n=1000,M=10000,K=6.RDS')[[2]],
    readRDS('res/cauchy/mean/not_perm,par2=(0.316227766016838,1),n=1000,M=10000,K=6.RDS')[[2]]
  )
  not_perm.thresholds.M10000 <- round(rbind(
    readRDS('res/cauchy/mean/not_perm,par2=(0.0632455532033676,1),n=1000,M=10000,K=6.RDS')[[1]][2:3],
    readRDS('res/cauchy/mean/not_perm,par2=(0.0948683298050514,1),n=1000,M=10000,K=6.RDS')[[1]][2:3],
    readRDS('res/cauchy/mean/not_perm,par2=(0.126491106406735,1),n=1000,M=10000,K=6.RDS')[[1]][2:3],
    readRDS('res/cauchy/mean/not_perm,par2=(0.221359436211787,1),n=1000,M=10000,K=6.RDS')[[1]][2:3],
    readRDS('res/cauchy/mean/not_perm,par2=(0.316227766016838,1),n=1000,M=10000,K=6.RDS')[[1]][2:3]
  ), 4)
  h <- c(2,3,4,7,10)
  MNKab <- round(power.MNKab(h, rbind(h, not_perm.M10000[,2]))[[1]], 4)*100
  MNKa <- round(power.MNKa(h, rbind(h, not_perm.M10000[,2]))[[1]], 4)*100
  n <- 1000
  analytic <- round(analytic.power(h, n)[[1]] * 100, 2)
  not_perm.M10000 <- not_perm.M10000[,2:3]*100
  
  cbind(h, L2_n.p=not_perm.M10000[,1], analytic, MNKab=MNKab, MNKa=MNKa, LLc_n.p=not_perm.M10000[,2], not_perm.thresholds.M10000)
}
tname <- 'tables/not_perm,n=1000,M=10000.tex'
cat('% ', file=tname)
write.table(res, tname, 
            quote = F, sep = ' & ', eol = ' \\\\\n', row.names = F, col.names = T, append = TRUE)
# h(c(2,3,4,7,10), 1000)

# n=1000,M=1000
res <- {
  not_perm.n1000 <- rbind(
    readRDS('res/cauchy/mean/not_perm,par2=(0.0316227766016838,1),n=1000,M=1000,K=6.RDS'),
    readRDS('res/cauchy/mean/not_perm,par2=(0.0632455532033676,1),n=1000,M=1000,K=6.RDS'),
    readRDS('res/cauchy/mean/not_perm,par2=(0.0948683298050514,1),n=1000,M=1000,K=6.RDS'),
    readRDS('res/cauchy/mean/not_perm,par2=(0.126491106406735,1),n=1000,M=1000,K=6.RDS'),
    readRDS('res/cauchy/mean/not_perm,par2=(0.221359436211787,1),n=1000,M=1000,K=6.RDS'),
    readRDS('res/cauchy/mean/not_perm,par2=(0.316227766016838,1),n=1000,M=1000,K=6.RDS')
  )
  L2LLc.n1000 <- rbind(
    readRDS('res/cauchy/mean/L2LLcpar2=(0.0316227766016838,1),n=1000,M=1000,D=800.RDS'),
    readRDS('res/cauchy/mean/L2LLcpar2=(0.0632455532033676,1),n=1000,M=1000,D=800.RDS'),
    readRDS('res/cauchy/mean/L2LLcpar2=(0.0948683298050514,1),n=1000,M=1000,D=800.RDS'),
    readRDS('res/cauchy/mean/L2LLcpar2=(0.126491106406735,1),n=1000,M=1000,D=800.RDS'),
    readRDS('res/cauchy/mean/L2LLcpar2=(0.221359436211787,1),n=1000,M=1000,D=800.RDS'),
    readRDS('res/cauchy/mean/L2LLcpar2=(0.316227766016838,1),n=1000,M=1000,D=800.RDS')
  )
  h <- c(1,2,3,4,7,10)
  MNKab <- round(power.MNKab(h, rbind(h, not_perm.n1000[,2]))[[1]], 4)
  MNKa <- round(power.MNKa(h, rbind(h, not_perm.n1000[,2]))[[1]], 4)
  n <- 1000
  analytic <- round(analytic.power(h, n)[[1]], 4)
  
  n1000 <- cbind(L2 = L2LLc.n1000[,1], L2_n.p = not_perm.n1000[,2], analytic, MNKab, MNKa, 
                 LLc_n.p = not_perm.n1000[,3], L2LLc.n1000[,2:3]) * 100
  cbind(h, n1000)
}
tname <- 'tables/L2LLc_not_perm,n=1000,M=1000,D=800.tex'
cat('% ', file=tname)
write.table(res, tname, 
            quote = F, sep = ' & ', eol = ' \\\\\n', row.names = F, col.names = T, append = TRUE)
# h(c(1,2,3,4,7,10), 1000)

# n = 500
res <- {
  not_perm.n500 <- rbind(
    readRDS('res/cauchy/mean/not_perm,par2=(0.0447213595499958,1),n=500,M=1000,K=6.RDS')[[2]],
    readRDS('res/cauchy/mean/not_perm,par2=(0.0894427190999916,1),n=500,M=1000,K=6.RDS')[[2]],
    readRDS('res/cauchy/mean/not_perm,par2=(0.134164078649987,1),n=500,M=1000,K=6.RDS')[[2]],
    readRDS('res/cauchy/mean/not_perm,par2=(0.223606797749979,1),n=500,M=1000,K=6.RDS')[[2]],
    readRDS('res/cauchy/mean/not_perm,par2=(0.313049516849971,1),n=500,M=1000,K=6.RDS')[[2]],
    readRDS('res/cauchy/mean/not_perm,par2=(0.402492235949962,1),n=500,M=1000,K=6.RDS')[[2]]
  )[,2:3]
  not_perm.thresholds.n500 <- round(rbind(
    readRDS('res/cauchy/mean/not_perm,par2=(0.0447213595499958,1),n=500,M=1000,K=6.RDS')[[1]],
    readRDS('res/cauchy/mean/not_perm,par2=(0.0894427190999916,1),n=500,M=1000,K=6.RDS')[[1]],
    readRDS('res/cauchy/mean/not_perm,par2=(0.134164078649987,1),n=500,M=1000,K=6.RDS')[[1]],
    readRDS('res/cauchy/mean/not_perm,par2=(0.223606797749979,1),n=500,M=1000,K=6.RDS')[[1]],
    readRDS('res/cauchy/mean/not_perm,par2=(0.313049516849971,1),n=500,M=1000,K=6.RDS')[[1]],
    readRDS('res/cauchy/mean/not_perm,par2=(0.402492235949962,1),n=500,M=1000,K=6.RDS')[[1]]
  )[,2:3], 4)
  L2LLc.n500 <- rbind(
    readRDS('res/cauchy/mean/L2LLcpar2=(0.0447213595499958,1),n=500,M=1000,D=800.RDS'),
    readRDS('res/cauchy/mean/L2LLcpar2=(0.0894427190999916,1),n=500,M=1000,D=800.RDS'),
    readRDS('res/cauchy/mean/L2LLcpar2=(0.134164078649987,1),n=500,M=1000,D=800.RDS'),
    readRDS('res/cauchy/mean/L2LLcpar2=(0.223606797749979,1),n=500,M=1000,D=800.RDS'),
    readRDS('res/cauchy/mean/L2LLcpar2=(0.313049516849971,1),n=500,M=1000,D=800.RDS'),
    readRDS('res/cauchy/mean/L2LLcpar2=(0.402492235949962,1),n=500,M=1000,D=800.RDS')
  )[,1:3] * 100
  h <- c(1,2,3,5,7,9)
  MNKab <- round(power.MNKab(h, rbind(h, not_perm.n500[,1]))[[1]], 4)*100
  MNKa <- round(power.MNKa(h, rbind(h, not_perm.n500[,1]))[[1]], 4)*100
  n <- 500
  analytic <- round(analytic.power(h, n)[[1]], 4)*100
  not_perm.n500 <- not_perm.n500*100
  
  cbind(h, L2=L2LLc.n500[,1], L2_n.p=not_perm.n500[,1], analytic, MNKab=MNKab, MNKa=MNKa, LLc_n.p=not_perm.n500[,2],
        not_perm.thresholds.n500, L2LLc.n500[,2:3])
}
tname <- 'tables/L2LLc_not_perm,n=500,M=1000,D=800.tex'
cat('% ', file=tname)
write.table(res, tname, 
            quote = F, sep = ' & ', eol = ' \\\\\n', row.names = F, col.names = T, append = TRUE)
# h(c(1,2,3,5,7,9), 500)
