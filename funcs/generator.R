source('exact.permutations.R')

get.A <- function(Z, n) {
  A <- 0
  for (i in 1:(2*n - 1))
    for (j in (i + 1):(2*n))
      A <- A + abs(Z[i] - Z[j])
  A / (n * (2*n - 1))
}

generate <- function(distr, n, par1, par2, exact = FALSE, folder, M = 10000, D = 1600) {
  path <- paste0('../data/', substring(as.character(substitute(distr)), 2))
  if (!dir.exists(path)) dir.create(path)
  path <- paste0(path, '/', folder)
  if (!dir.exists(path)) dir.create(path)

  n.par1 <- length(par1)
  n.par2 <- length(par2)
  if (n.par1 != n.par2) {
    if (n.par1 > n.par2) par2 <- rep(par2, n.par1) else par1 <- rep(par1, n.par2)
  }
  n.par = max(n.par1, n.par2)
  
  sapply(1:n.par, function(i.par) {
    sapply(1:M, function(i) {
      Z <- c(distr(n, par1[1], par2[1]), distr(n, par1[i.par], par2[i.par]))
      
      Z.perm <- t(if (exact) exact.permutations(Z[1:n], Z[(n+1):(2*n)], n, n) else replicate(D,sample(Z)))
      
      A <- get.A(Z, n)
      
      D.exact = if (exact) ',exact' else paste0(',D=', D)
      current_path = paste0(path, '/n=', n, ',M=', M, D.exact, ',par1=', par1[1], '_', par2[1], ',par2=', par1[i.par], '_', par2[i.par])
      if (!dir.exists(current_path)) dir.create(current_path)
      saveRDS(list(Z = Z, Z.perm = Z.perm, A = A), paste0(current_path, '/', i, '.RDS'))
    })
  })
  return()
}
