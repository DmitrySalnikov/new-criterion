source('/home/d/1/new_criteria/funcs/funcs.R')

merge.res <- function(distribution, type, par2, n = 50, M = 1000, D = 800, prefix) {
  res.name <- make.res.name(distribution, type, par2, n, M, D, prefix = NULL)
  prefix.res.name <- make.res.name(distribution, type, par2, n, M, D, prefix)
  res <- readRDS(res.name)
  prefix.res <- readRDS(prefix.res.name)
  
  saveRDS(c(res[1:6], prefix.res[2], res[7:14], prefix.res[4:5]), res.name)
  unlink(prefix.res.name)
}

merge.res.common <- function(distribution, par, n = 50, M = 1000, D = 800, prefix) {
  merge.res(distribution, 'mean', c(0, 1), n, M, D, prefix)
  for (x in par[-1]) {
    merge.res(distribution, 'mean', c(x, 1), n, M, D, prefix)
    merge.res(distribution, 'var', c(0, 1 + 2 * x), n, M,  D, prefix)
    merge.res(distribution, 'mean1var', c(x, 1 + x), n, M, D, prefix)
    merge.res(distribution, 'mean2var', c(x, 1 + 2 * x), n, M, D, prefix)
  }  
}

merge.res.common('laplace', seq(0, 1, length.out = 5), M = 10000, D = 1600, prefix = 'Linf')
