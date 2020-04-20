source('/home/d/1/new_criteria/funcs/funcs.R')

calculate <- function(distribution, par, n = 50, M = 1000, D = 800, prefix = NULL) {
  Power(distribution, 'mean', c(0, 1), n = n, M = M, D = D, prefix = prefix)
  for (x in par[-1]) {
    Power(distribution, 'mean', c(x, 1), n = n, M = M, D = D, prefix = prefix)
    Power(distribution, 'var', c(0, 1 + 2 * x), n = n, M = M, D = D, prefix = prefix)
    Power(distribution, 'mean1var', c(x, 1 + x), n = n, M = M, D = D, prefix = prefix)
    Power(distribution, 'mean2var', c(x, 1 + 2 * x), n = n, M = M, D = D, prefix = prefix)
  }  
}

calculate('norm', seq(0, 1, length.out = 5))
calculate('cauchy', seq(0, 1, length.out = 5))
calculate('levy', seq(0, 1, length.out = 5))
calculate('logcauchy', seq(0, 1, length.out = 5))
calculate('laplace', seq(0, 1, length.out = 5), M = 10000, D = 1600)

calculate('norm', seq(0, 1, length.out = 5), prefix = 'LLlevy')
calculate('cauchy', seq(0, 1, length.out = 5), prefix = 'LLlevy')
calculate('logcauchy', seq(0, 1, length.out = 5), prefix = 'LLlevy')
calculate('laplace', seq(0, 1, length.out = 5), M = 10000, D = 1600, prefix = 'LLlevy')

# Power('norm', 'temp', c(0,1), prefix = 'LLlevy', n = 10, M = 100, D = 100)
