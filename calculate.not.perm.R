source('/home/d/1/new_criteria/funcs/Power.not.perm.R')
n <- 100
v <- seq(3,12,3) / sqrt(n)
for (i in v) {
  Power.cauchy.not.perm('var', c(0, 1 + i), n=n)
}