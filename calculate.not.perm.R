source('/home/d/1/new_criteria/funcs/Power.not.perm.R')
# n <- 100
# v <- seq(3,15,3) / sqrt(n)
# for (i in v) {
#   Power.cauchy.not.perm('var', c(0, 1 + i), n=n)
# }
#
# n <- 1000
# v <- seq(3,15,3) / sqrt(n)
# for (i in v) {
#   Power.cauchy.not.perm('var', c(0, 1 + i), n=n)
# }

# source('/home/d/1/new_criteria/funcs/funcs.R')
#
# n <- 100
# v <- seq(3,15,3) / sqrt(n)
# for (i in v) {
#   Power('cauchy', 'var', c(0, 1 + i), n = n, prefix = "L2", randomization = FALSE)
# }
#
# n <- 500
# v <- seq(3,15,3) / sqrt(n)
# for (i in v) {
#   Power('cauchy', 'var', c(0, 1 + i), n = n, prefix = "L2", randomization = FALSE)
# }
#
# n <- 1000
# v <- seq(3,15,3) / sqrt(n)
# for (i in v) {
#   Power('cauchy', 'var', c(0, 1 + i), n = n, prefix = "L2", randomization = FALSE)
# }

# par <- seq(3,15,3)

make.table <- function(n) {
  n.p.res <- c()
  res <- c()
  var <- 1 + par / sqrt(n)
  for (i in var) {
    n.p.res <- rbind(n.p.res, readRDS(paste0('res/cauchy/var/not_perm,par2=(0,', i, '),n=', n, ',M=1000,K=6.RDS'))[[2]][-1])
    res <- rbind(res, readRDS(paste0('res/cauchy/var/L2par2=(0,', i, '),n=', n, ',M=1000,D=800.RDS'))[1])
  }
  table <- cbind(as.character(par), res, n.p.res)
  table.name <- paste0('tables/L2p.n.p,var,n=', n, ',M=1000,D=800,K=6.tex')
  cat('% ', file = table.name)
  write.table(table, table.name,
              quote = F, sep = ' & ', eol = ' \\\\\n',
              row.names = F, col.names = col.names, append = T)
  # write('\\hline', table.name, append = TRUE)
}
# make.table(100)
# make.table(500)
# make.table(1000)

source('/home/d/1/new_criteria/funcs/funcs.R')

col.names <- c('$h$', '$L2$', "$n.pL2$", "$wilcox.test$", '$ks.test$')
# par <- c(2,4,6,8,10)

# for (n in c(1000, 500, 100)) {
  # v <- par / sqrt(n)
  # for (i in v) {
  #   Power.cauchy.not.perm('var', c(0, 1 + i), n=n)
  #   Power('cauchy', 'var', c(0, 1 + i), n = n, prefix = "L2", randomization = FALSE)
  # }
#   make.table(n)
# }

par <- c(1,2,3,5,7,9)
type <- 'mean'
for (n in c(100)) {
  v <- par / sqrt(n)
  for (i in v) {
    Power.cauchy.not.perm('mean', c(i, 1), n=n)
    Power('cauchy', 'mean', c(i, 1), n = n, prefix = "L2", randomization = FALSE)
  }
#   make.table(n)
}