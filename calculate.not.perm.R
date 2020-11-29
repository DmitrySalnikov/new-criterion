source('/home/d/1/new_criteria/funcs/Power.not.perm.R')
source('/home/d/1/new_criteria/funcs/funcs.R')

make.table <- function(n) {
  n.p.res <- c()
  res <- c()
  if (type == 'var') {
    par2 <- paste0('0,', as.character(1 + par / sqrt(n)))
  } else if (type == 'mean') {
    par2 <- paste0(as.character(par / sqrt(n)), ',1')
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
  table.name <- paste0('tables/L2p.n.p,', distr, ',', type, ',n=', n, ',M=1000,D=800,K=6.tex')
  cat('% ', file = table.name)
  write.table(table, table.name,
              quote = F, sep = ' & ', eol = ' \\\\\n',
              row.names = F, col.names = col.names, append = T)
  write('\\hline', table.name, append = TRUE)
}


col.names <- c('$h$', '$T_n, perm$', "$T_n, sim$", "$wilcox.test$", '$ks.test$')

par <- c(2,4,6,8,10)
distr <- 'cauchy'
type <- 'var'
for (n in c(1000, 500)) {
  make.table(n)
}
# for (n in c(1000, 500, 100)) {
  # v <- par / sqrt(n)
  # for (i in v) {
  #   Power.cauchy.not.perm('var', c(0, 1 + i), n=n)
  #   Power('cauchy', 'var', c(0, 1 + i), n = n, prefix = "L2", randomization = FALSE)
  # }
#   make.table(n)
# }

# par <- c(1,2,3,5,7,9)
# distr <- 'cauchy'
# type <- 'mean'

# par <- c(9)
# for (n in c(1000)) {
#   v <- par / sqrt(n)
#   for (i in v) {
#     Power.not.perm(distr, type, c(i, 1), n=n)
#     Power(distr, type, c(i, 1), n = n, prefix = "L2", randomization = FALSE)
#   }
# }

# par <- c(1,2,3,5,7,9)
# make.table(1000)

par <- c(1,2,3,4,5)
distr <- 'norm'
type <- 'mean'
for (n in c(500)) {
  v <- par / sqrt(n)
  for (i in v) {
    # Power.not.perm(distr, type, c(i, 1), n = n)
    Power(distr, type, c(i, 1), n = n, prefix = "L2", randomization = FALSE)
  }
  # make.table(n)
}

par <- c(1,2,3,4,5)
distr <- 'norm'
type <- 'var'
for (n in c(500)) {
  v <- par / sqrt(n)
  for (i in v) {
    # Power.not.perm(distr, type, c(0, 1 + i), n = n)
    Power(distr, type, c(0, 1 + i), n = n, prefix = "L2", randomization = FALSE)
  }
  # make.table(n)
}