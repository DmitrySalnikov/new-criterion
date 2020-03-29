path = '/home/d/1/new_criteria'

n <- 50
M <- 10000
D <- 1600
first <- seq(0, 1, length.out = 5)
details <- paste0('par2=(', first, ',', 1, '),n=', n, ',M=', M, ',D=', D)
res <- vecor(0) 
for (x in details)
  res <- rbind(res, readRDS(paste0(path, '/res/laplace/mean/', x, '.RDS')))
res
