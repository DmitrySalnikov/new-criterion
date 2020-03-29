source('/home/d/1/new_criteria/funcs/funcs.R')

read.res('laplace', cbind(0, 1), 'mean')

par <- seq(0, 1, length.out = 5)[-1]
make.table('laplace', tests.numbers = c(3, 5, 7, 10:14), list(
  cbind(0, 1), 
  list(cbind(par, 1), 'mean'), 
  list(cbind(0, seq(1.5, 3, length.out = 4)), 'var'),
  list(cbind(par, 1 + par), 'mean1var'), 
  list(cbind(par, 1 + 2 * par), 'mean2var')
))
