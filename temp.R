source('/home/d/1/new_criteria/funcs/funcs.R')

for (first in seq(0.5, 1, length.out = 3)) {
  Power('laplace', c(first, 1), 'mean')
}
