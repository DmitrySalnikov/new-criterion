source('/home/d/1/new_criteria/funcs/funcs.R')

calculate <- function(distribution, par, n = 50, M = 1000, D = 800, prefix = NULL, randomization = TRUE) {
  Power(distribution, 'mean', c(0, 1), n = n, M = M, D = D, prefix = prefix, randomization = randomization)
  for (x in par[-1]) {
    Power(distribution, 'mean', c(x, 1), n = n, M = M, D = D, prefix = prefix, randomization = randomization)
    Power(distribution, 'var', c(0, 1 + 2 * x), n = n, M = M, D = D, prefix = prefix, randomization = randomization)
    Power(distribution, 'mean1var', c(x, 1 + x), n = n, M = M, D = D, prefix = prefix, randomization = randomization)
    Power(distribution, 'mean2var', c(x, 1 + 2 * x), n = n, M = M, D = D, prefix = prefix, randomization = randomization)
  }
}

calculate('norm', seq(0, 1, length.out = 5), randomization = FALSE)
calculate('cauchy', seq(0, 1, length.out = 5), randomization = FALSE)
calculate('levy', seq(0, 1, length.out = 5), randomization = FALSE)
calculate('logcauchy', seq(0, 1, length.out = 5), randomization = FALSE)
calculate('laplace', seq(0, 1, length.out = 5), M = 10000, D = 1600, randomization = FALSE)

# calculate('norm', seq(0, 1, length.out = 5), prefix = 'LLlevy', randomization = FALSE)
# calculate('cauchy', seq(0, 1, length.out = 5), prefix = 'LLlevy', randomization = FALSE)
# calculate('logcauchy', seq(0, 1, length.out = 5), prefix = 'LLlevy', randomization = FALSE)
# calculate('laplace', seq(0, 1, length.out = 5), M = 10000, D = 1600, prefix = 'LLlevy', randomization = FALSE)

calculate('norm', seq(0, 1, length.out = 5), randomization = TRUE)
calculate('cauchy', seq(0, 1, length.out = 5), randomization = TRUE)
calculate('levy', seq(0, 1, length.out = 5), randomization = TRUE)
calculate('logcauchy', seq(0, 1, length.out = 5), randomization = TRUE)
calculate('laplace', seq(0, 1, length.out = 5), randomization = TRUE)

Power('norm', 'mean', c(0, 1), n = 5, randomization = FALSE)
Power('cauchy', 'mean', c(0, 1), n = 5, randomization = FALSE)
Power('levy', 'mean', c(0, 1), n = 5, randomization = FALSE)
Power('logcauchy', 'mean', c(0, 1), n = 5, randomization = FALSE)
Power('laplace', 'mean', c(0, 1), n = 5, randomization = FALSE)

Power('norm', 'mean', c(0, 1), n = 5, randomization = TRUE)
Power('cauchy', 'mean', c(0, 1), n = 5, randomization = TRUE)
Power('levy', 'mean', c(0, 1), n = 5, randomization = TRUE)
Power('logcauchy', 'mean', c(0, 1), n = 5, randomization = TRUE)
Power('laplace', 'mean', c(0, 1), n = 5, randomization = TRUE)

# calculate('norm', seq(0, 1, length.out = 5), prefix = 'L2s', randomization = FALSE)
# calculate('cauchy', seq(0, 1, length.out = 5), prefix = 'L2s', randomization = FALSE)
# calculate('levy', seq(0, 1, length.out = 5), prefix = 'L2s', randomization = FALSE)
# calculate('logcauchy', seq(0, 1, length.out = 5), prefix = 'L2s', randomization = FALSE)
calculate('laplace', seq(0, 1, length.out = 5), randomization = FALSE)

calculate('cauchy', seq(0, 0.4, length.out = 5), n = 200, M = 1000, D = 800, prefix = NULL, randomization = FALSE)

Power('cauchy', 'mean', c(0, 1), n = 1000, prefix = "L2LLc", randomization = FALSE)
Power('cauchy', 'mean', c(0.05, 1), n = 1000, prefix = "L2LLc", randomization = FALSE)
Power('cauchy', 'mean', c(0.1, 1), n = 1000, prefix = "L2LLc", randomization = FALSE)
Power('cauchy', 'mean', c(0.15, 1), n = 1000, prefix = "L2LLc", randomization = FALSE)
Power('cauchy', 'mean', c(0.2, 1), n = 1000, prefix = "L2LLc", randomization = FALSE)

Power('cauchy', 'mean', c(0.1, 1), n = 5000, prefix = "L2LLc", randomization = FALSE)
Power('cauchy', 'mean', c(0.2, 1), n = 1250, prefix = "L2LLc", randomization = FALSE)
Power('cauchy', 'mean', c(0.5, 1), n = 200, prefix = "L2LLc", randomization = FALSE)

Power('cauchy', 'mean', c(1/sqrt(1000), 1), n = 1000, prefix = "L2LLc", randomization = FALSE)
Power('cauchy', 'mean', c(2/sqrt(1000), 1), n = 1000, prefix = "L2LLc", randomization = FALSE)
Power('cauchy', 'mean', c(3/sqrt(1000), 1), n = 1000, prefix = "L2LLc", randomization = FALSE)
Power('cauchy', 'mean', c(4/sqrt(1000), 1), n = 1000, prefix = "L2LLc", randomization = FALSE)
Power('cauchy', 'mean', c(7/sqrt(1000), 1), n = 1000, prefix = "L2LLc", randomization = FALSE)
Power('cauchy', 'mean', c(10/sqrt(1000), 1), n = 1000, prefix = "L2LLc", randomization = FALSE)

Power('cauchy', 'mean', c(1/sqrt(500), 1), n = 500, prefix = "L2LLc", randomization = FALSE)
Power('cauchy', 'mean', c(2/sqrt(500), 1), n = 500, prefix = "L2LLc", randomization = FALSE)
Power('cauchy', 'mean', c(3/sqrt(500), 1), n = 500, prefix = "L2LLc", randomization = FALSE)
Power('cauchy', 'mean', c(5/sqrt(500), 1), n = 500, prefix = "L2LLc", randomization = FALSE)
Power('cauchy', 'mean', c(7/sqrt(500), 1), n = 500, prefix = "L2LLc", randomization = FALSE)
Power('cauchy', 'mean', c(9/sqrt(500), 1), n = 500, prefix = "L2LLc", randomization = FALSE)