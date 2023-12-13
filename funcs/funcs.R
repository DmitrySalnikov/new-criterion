path = 'C:/Users/Professional/Desktop/new_criteria'

library(kSamples)
library(foreach)
library(doParallel)

cores <- detectCores()

source(paste(path, 'funcs', 'distributions.R', sep = '/'))
source(paste(path, 'funcs', 'exact.permutations.R', sep = '/'))
source(paste(path, 'funcs', 'static.R', sep = '/'))

DISTRIBUTIONS <- c('norm', 'cauchy', 'levy', 'laplace')
LOG.DISTRIBUTIONS <- c('loglaplace', 'logcauchy')

L.test.stat <- function(x, y, A) {
  diff <- sapply(x, function(x.i) { abs(x.i - y) })
  dim(diff) <- NULL
  # diff.A <- diff / A
  L2 <- sum(log(1 + diff**2))
  #U2 <- sum(diff**2)
  # B1 <- sum(log(1 + sapply(x, function(x.i) { abs(x.i - x) })**2)) / n / (n-1)
  # B2 <- sum(log(1 + sapply(y, function(y.i) { abs(y.i - y) })**2)) / n / (n-1)
  c(
    # L05 = sum(log(1 + diff**.5)),
    # L05C = sum(log(1 + diff.A**.5)),
    # L1 = sum(log(1 + diff)),
    # L1C = sum(log(1 + diff.A)),
    L2 = L2 # ,
    # U2 = U2,
    # L2C = sum(log(1 + diff.A**2)),
    # L2s = L2 / n**2 - sqrt(B1 * B2),
    # Linf = sum(log(diff))
  )
}

Power <- function(distribution, type, par2, par1 = c(0, 1), n = 100, M = 1000, D = 800, alpha = 0.05, prefix = NULL, randomization = FALSE) {
  start.time <- Sys.time()

  exact <- n == 5
  if(exact) {
    D <- n.exact.perms(5, 5, 10)
  }

  n <<- n
  n.permutations <<- D
  alpha <<- alpha
  randomization <<- randomization

  details <- paste0(prefix, 'par2=(', par2[1], ',', par2[2], '),n=', n, ',M=', M, ',D=', D)
  if (exact) {
    details <- paste0(details, ',exact')
  }
  if (randomization) {
    details <- paste0(details, ',rand')
  }
  
  data.path <- create.folder(c(path, 'data', distribution, type, details))
  res.path <- create.folder(c(path, 'res', distribution, type))
  res.name <- paste0(res.path, '/', details, '.RDS')
  
  if (file.exists(res.name)) {
    return()
  }

  print(paste0(distribution, ', ', details, ', ', round(as.numeric(Sys.time() - start.time, units="mins"), 1)))

  set.seed(500)
  X.set <- get(paste0('r', distribution))(n * M, par1[1], par1[2])
  Y.set <- get(paste0('r', distribution))(n * M, par2[1], par2[2])
  dim(X.set) <- c(M, n)
  dim(Y.set) <- c(M, n)
  Z.set <- cbind(X.set, Y.set)
  if (!dir.exists(data.path) || length(dir(data.path)) != M) {
    for (i in 1:M) {
      if (exact) {
        saveRDS(t(exact.permutations(X.set[i, ], Y.set[i, ], 5, 5)), paste0(data.path, '/', i, '.RDS'))
      } else {
        saveRDS(t(replicate(D, sample(Z.set[i, ]))), paste0(data.path, '/', i, '.RDS'))
      }
    }
  }
  # A.set <- apply(Z.set, 1, get.A)
  
  cluster <- makeCluster(cores - 1)
  registerDoParallel(cluster)
  res <- rowMeans(sapply(1:M, function(i) {
    if (i %% 100 == 0) {
      print(paste0(i, ', ', round(as.numeric(Sys.time() - start.time, units="mins"), 1)))
    }

    X <- X.set[i, ]
    Y <- Y.set[i, ]
    Z <- Z.set[i, ]
    
    permutations <- readRDS(paste0(data.path, '/', i, '.RDS'))
    A <- NULL# A <- A.set[i]

    res <- c(
      L.test(X, Y, A, permutations)#,
      # LLnorm = LL.test(X, Y, Z, 'norm', permutations),
      # LLnorm.var.eq = LL.test(X, Y, Z, 'norm', permutations, var.equal = TRUE),
      # LLnorm.mean.eq = LL.test(X, Y, Z, 'norm', permutations, mean.equal = TRUE),
      # LLcauchy = LL.test(X, Y, Z, 'cauchy', permutations)#,
      # LLlaplace = LL.test(X, Y, Z, 'laplace', permutations),
      # LLlevy = LL.test(X, Y, Z, 'levy', permutations)
    )

    if (distribution %in% LOG.DISTRIBUTIONS) {
      res <- c(res,
         LLlogcauchy = LL.test(X, Y, Z, 'logcauchy', permutations),
         LLloglaplace = LL.test(X, Y, Z, 'loglaplace', permutations)
      )
    }

    if (!randomization) {
      res <- res < alpha
    }

    c(res,
      c(
        wilcox.test = wilcox.test(X, Y)$p.value,
        ks.test     = ks.test(X, Y)$p.value,
        ad.test     = ad.test(X, Y, method = "asymptotic")$ad[1,3],
        t.test      = t.test(X, Y)$p.value,
        var.test    = var.test(X, Y)$p.value
      ) < alpha
    )
  }))
  stopCluster(cluster)

  print(res)

  saveRDS(res, res.name)

  unlink(data.path, recursive = TRUE)
}

MakeTable <- function(idx1 = vector(), with_F1 = FALSE) {
  if (length(idx1)) res <- res[,c(1,2,idx1)]
  if (!with_F1) res <- res[,-1]

  cat('% ', file = paste0(tables, tname, '.tex'))
  write.table(res, paste0(tables, tname, '.tex'),
              quote = F, sep = ' & ', eol = ' \\\\\n',
              row.names = F, col.names = T, append = TRUE)
  write('\\hline', paste0(tables, tname, '.tex'), append = TRUE)
}
