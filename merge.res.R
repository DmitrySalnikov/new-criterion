source('/home/d/1/new_criteria/funcs/funcs.R')

merge.res <- function(distribution, type, par2, n = 50, M = 1000, D = 800, prefix, randomization = TRUE) {
  res.name <- make.res.name(distribution, type, par2 = par2, n, M, D, prefix = NULL, randomization = randomization)
  prefix.res.name <- make.res.name(distribution, type, par2, n, M, D, prefix, randomization = randomization)
  res <- readRDS(res.name)
  prefix.res <- readRDS(prefix.res.name)
  
  saveRDS(c(res[1:6], prefix.res[1], res[7:17]), res.name)
  unlink(prefix.res.name)
  
  # print(res)
  # print(prefix.res)
  # print(c(res[1:6], prefix.res[1], res[7:17]))
}

merge.res.common <- function(distribution, par, n = 50, M = 1000, D = 800, prefix, randomization) {
  merge.res(distribution, 'mean', c(0, 1), n, M, D, prefix, randomization)
  for (x in par[-1]) {
    merge.res(distribution, 'mean', c(x, 1), n, M, D, prefix, randomization)
    merge.res(distribution, 'var', c(0, 1 + 2 * x), n, M,  D, prefix, randomization)
    merge.res(distribution, 'mean1var', c(x, 1 + x), n, M, D, prefix, randomization)
    merge.res(distribution, 'mean2var', c(x, 1 + 2 * x), n, M, D, prefix, randomization)
  }  
}

# merge.res.common('laplace', seq(0, 1, length.out = 5), M = 10000, D = 1600, prefix = 'Linf')

# merge.res.common('levy', seq(0, 1, length.out = 5), prefix = 'LLlevy')
# merge.res.common('norm', seq(0,1,length.out = 5), prefix = 'LLlevy')
# merge.res.common('cauchy', seq(0,1,length.out = 5), prefix = 'LLlevy')
# merge.res.common('logcauchy', seq(0,1,length.out = 5), prefix = 'LLlevy')
# merge.res.common('laplace', seq(0,1,length.out = 5), M = 10000, D = 1600, prefix = 'LLlevy')

merge.res.common('norm', seq(0, 1, length.out = 5), prefix = 'L2s', randomization = FALSE)
merge.res.common('cauchy', seq(0, 1, length.out = 5), prefix = 'L2s', randomization = FALSE)
merge.res.common('levy', seq(0, 1, length.out = 5), prefix = 'L2s', randomization = FALSE)
merge.res.common('logcauchy', seq(0, 1, length.out = 5), prefix = 'L2s', randomization = FALSE)
