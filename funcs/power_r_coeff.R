# cauchy
res = c()
h1s = 0:10
for(h1 in h1s) {
  z = qnorm(0.975)
  
  a = 0.8955417
  b = h1 / 3
  # b = a * h1 / 3
  # r = 2 * a**2 * h1 / 3
  r = 2 * a * h1 / 3
  # r = 2 * h1 / 3
  
  n = 1000000
  L = rnorm(n)
  p = (a * L) ** 2 + r * L + b ** 2 - (a * z) ** 2 >= 0
  res = c(res, sum(p) / n)
}
rbind(h1s, round(res * 100, 1))

res = c()
h1s = 0:10
for(h1 in h1s) {
  z = qnorm(0.975)
  
  a = 0.8955417
  b = h1 / 3
  
  n = 1000000
  L = rnorm(n)
  p = (a * L + b) ** 2 - (a * z) ** 2 >= 0
  res = c(res, sum(p) / n)
}
rbind(h1s, round(res * 100, 1))

# norm
res = c()
h1s = seq(0, 5, 0.5)
for(h1 in h1s) {
  z = qnorm(0.975)
  
  a = 0.7302345
  b = 0.3990376 * h1
  # b = 0.2821622 * h1
  # b = 0.38414035104 * h1

  n = 1000000
  L = rnorm(n)
  p = (a * L + b) ** 2 - (a * z) ** 2 >= 0
  res = c(res, sum(p) / n)
}
rbind(h1s, round(res * 100, 1))
