source('./funcs/Power.not.perm.R')
source('./funcs/funcs.R')

make.table <- function(n) {
  n.p.res <- c()
  res <- c()
  if (type == 'var') {
    par2 <- paste0('0,', as.character(1 + par / sqrt(n)))
  } else if (type == 'mean') {
    par2 <- paste0(as.character(par * 0.1 / sqrt(n)), ',0.1')
  }
  for (i in par2) {
    n.p.res <- rbind(n.p.res, round(readRDS(paste0('res/', distr, '/', type, '/not_perm,par2=(', i, '),n=', n, ',M=1000,K=6.RDS'))[[2]][-1] * 100, 1))
    res <- rbind(res, round(readRDS(paste0('res/', distr, '/', type, '/L2par2=(', i, '),n=', n, ',M=1000,D=800.RDS'))[1] * 100, 1))
  }
  if (distr == 'cauchy' && type == 'mean') {
    col.names <- c(col.names[1:3], 'formulae', col.names[-(1:3)])
    n.p.res <- cbind(n.p.res[,1], round(new.analytic.power(par) * 100, 1), n.p.res[,-1])
  }
  table <- cbind(as.character(par), res, n.p.res)
  print(table)
  table.name <- paste0('tables/L2p.n.p,var=0.1,', distr, ',', type, ',n=', n, ',M=1000,D=800,K=6.tex')
  print(table.name)
  cat('% ', file = table.name)
  write.table(table, table.name,
              quote = F, sep = ' & ', eol = ' \\\\\n',
              row.names = F, col.names = col.names, append = T)
  write('\\hline', table.name, append = TRUE)
}


col.names <- c('$h$', '$T_n, perm$', "$T_n, sim$", "$wilcox.test$", '$ks.test$')

###########################################################################################################################

par <- c(0, 0.5)
distr <- 'norm'
type <- 'mean'
n <- 1000
# for (n in c(1000)) {
  v <- par / sqrt(n)
  for (i in v) {
    Power.not.perm(distr, type, c(i, 1), n = n)
    # Power(distr, type, c(i, 1), n = n, prefix = "tempL2")
  }
  # make.table(n)
# }
par <- sort(c(0:6 + 0.5, 0:6))
res <- c()
for(p in par) {
  res <- rbind(res, readRDS(paste0('res/norm/mean/not_perm,par2=(',p / sqrt(n),',1),n=1000,M=1000,K=6.RDS'))[[2]][1:4])
}

Q <- function(h, b) {
  1 - pnorm(qnorm(0.975) - b*h) + pnorm(-qnorm(0.975) - b*h)
}

fn <- function(b) {
  sum((Q(par, b) - res[, 2])**2)
}
round(Q(par, optim(1, fn, method = 'L-BFGS-B', lower = 0)$par), 3)*100
r <- res[,2]
names(r) <- par
r*100

###########################################################################################################################

par <- sort(c(0:8, 1:9 - 0.5))
distr <- 'norm'
type <- 'var'
n <- 1000
# for (n in c(1000)) {
v <- par / sqrt(n)
for (i in v) {
  Power.not.perm(distr, type, c(0, 1 + i), n = n)
  # Power(distr, type, c(i, 1), n = n, prefix = "tempL2")
}
# make.table(n)
# }
res <- c()
for(p in par) {
  res <- rbind(res, readRDS(paste0('res/norm/var/not_perm,par2=(0,', 1 + p / sqrt(n), '),n=1000,M=1000,K=6.RDS'))[[2]][1:4])
}
row.names(res) <- par

Q <- function(q, k) {
  1 - pnorm(qnorm(0.975) * (1 + k * q)) + pnorm(-qnorm(0.975) * (1 + k * q))
}

fn <- function(k) {
  sum((Q(par, k) - res[, 2])**2)
}
round(Q(par, optim(1, fn, method = 'L-BFGS-B', lower = 0)$par), 3)*100
r <- res[,2]
names(r) <- par
r*100

###########################################################################################################################

par <- c(0, 0.5, 1.5, 2.5, 3.5, 4, 4.5, 5.5, 6, 6.5, 7.5, 8, 8.5, 9.5, 10, 10.5)
distr <- 'cauchy'
type <- 'mean'

n <- 1000
# for (n in c(100)) {
  v <- par / sqrt(n)
  for (i in v) {
    Power.not.perm(distr, type, par2=c(i, 1), n=n)
    # Power(distr, type, par2=c(i, 0.1), par1=c(0,0.1), n = n, prefix = "L2", randomization = FALSE)
  }
# }
par_old <- c(1,2,3,4,7,10)
res <- c()
for(p in par_old) {
  res <- rbind(res, readRDS(paste0('res/cauchy/mean/not_perm,par2=(',p / sqrt(n),',1),n=1000,M=1000,K=6.RDS')))
}
# row.names(res) <- par_old
par <- sort(c(0:10 + 0.5, 0:10))
par <- par[!par %in% par_old]
for(p in par) {
  res <- rbind(res, readRDS(paste0('res/cauchy/mean/not_perm,par2=(',p / sqrt(n),',1),n=1000,M=1000,K=6.RDS'))[[2]][1:4])
}
row.names(res) <- c(par_old, par)
res <- res[order(as.numeric(row.names(res))),]

par <- sort(c(0:10 + 0.5, 0:10))
Q <- function(h, b) {
  1 - pnorm(qnorm(0.975) - b*h) + pnorm(-qnorm(0.975) - b*h)
}

fn <- function(b) {
  sum((Q(par, b) - res[, 2])**2)
}
round(Q(par, optim(1, fn, method = 'L-BFGS-B', lower = 0)$par), 3)*100
r <- res[,2]
names(r) <- par
r*100
  
###########################################################################################################################

par <- sort(c(0:13, 0:12 + 0.5))
distr <- 'cauchy'
type <- 'var'

n <- 1000
# for (n in c(100)) {
v <- par / sqrt(n)
for (i in v) {
  print(i, eol = '\n')
  Power.not.perm(distr, type, par2=c(0, 1 + i), n=n)
  # Power(distr, type, par2=c(i, 0.1), par1=c(0,0.1), n = n, prefix = "L2", randomization = FALSE)
}
# }
res <- c()
for(p in par) {
  res <- rbind(res, readRDS(paste0('res/cauchy/var/not_perm,par2=(0,', 1 + p / sqrt(n), '),n=1000,M=1000,K=6.RDS'))[[2]][1:4])
}
row.names(res) <- par

Q <- function(h, b) {
  1 - pnorm(qnorm(0.975) - b*h) + pnorm(-qnorm(0.975) - b*h)
}

fn <- function(b) {
  sum((Q(par, b) - res[, 2])**2)
}
round(Q(par, optim(1, fn, method = 'L-BFGS-B', lower = 0)$par), 3)*100
r <- res[,2]
names(r) <- par
r*100

# make.table(100)