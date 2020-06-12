source('/home/d/1/new_criteria/funcs/funcs.R')

short.names <- c(cauchy = 'C', norm = 'N', levy = 'Le', laplace = 'La', logcauchy = 'LC')
F2 <- function(distribution, par2) {
  paste0(short.names[distribution], '(', par2[1], ', ', par2[2], ')')
}

make.table.name <- function(distribution, n, M, D, randomization = TRUE, prefix = NULL) {
  exact <- n == 5
  if(exact) {
    D <- n.exact.perms(5, 5, 10)
  }
  details = paste0(path, '/tables/', prefix, distribution, ',n=', n, ',M=', M, ',D=', D)
  if(exact) {
    details <- paste0(details, ',exact')
  }
  if(randomization) {
    details = paste0(details, ',rand')
  }
  
  paste0(details, '.tex')
}

write.row <- function(distribution, type, par2, tests.numbers, n = 50, M = 1000, D = 800, col.names = FALSE, randomization = TRUE, prefix = NULL) {
  res <- c(F2(distribution, par2),
           round(read.res(distribution, type, par2, n, M, D, randomization = randomization, prefix = prefix)[tests.numbers], 3) * 100)
  table.name <- make.table.name(distribution, n, M, D, randomization, prefix = prefix)
  if (col.names) {
    cat('% ', file = table.name)
  }
  write.table(t(res), table.name, 
              quote = F, sep = ' & ', eol = ' \\\\\n',
              row.names = F, col.names = col.names, append = TRUE)
}

make.table <- function(distribution, par, tests.numbers, n = 50, M = 1000, D = 800, randomization = TRUE) {
  table.name <- make.table.name(distribution, n, M, D, randomization)
  write.row(distribution, 'mean', c(0, 1), tests.numbers, n, M, D, TRUE, randomization)
  write('\\hline', table.name, append = TRUE)
  for (x in par[-1]) {
    write.row(distribution, 'mean', c(x, 1), tests.numbers, n, M, D, randomization = randomization)
  }
  write('\\hline', table.name, append = TRUE)
  for (x in par[-1]) {
    write.row(distribution, 'var', c(0, 1 + 2 * x), tests.numbers, n, M,  D, randomization = randomization)
  }
  write('\\hline', table.name, append = TRUE)
  for (x in par[-1]) {
    write.row(distribution, 'mean1var', c(x, 1 + x), tests.numbers, n, M, D, randomization = randomization)
  }
  write('\\hline', table.name, append = TRUE)
  for (x in par[-1]) {
    write.row(distribution, 'mean2var', c(x, 1 + 2 * x), tests.numbers, n, M, D, randomization = randomization)
  }  
  write('\\hline', table.name, append = TRUE)
}

read.res('laplace', 'mean', cbind(1, 1), randomization = FALSE)
read.res('logcauchy', 'mean', cbind(1, 1))
read.res('norm', 'mean', cbind(0, 1), randomization = FALSE)
read.res('logcauchy', 'mean', cbind(0, 1), randomization = FALSE)
read.res('cauchy', 'mean', cbind(0, 1), randomization = TRUE)
read.res('levy', 'mean', cbind(0, 1), randomization = FALSE)
read.res('norm', 'mean', cbind(0, 1), randomization = TRUE)
read.res('cauchy', 'var', cbind(0, 1.8), randomization = FALSE, n = 200)

make.table('norm', seq(0, 1, length.out = 5), c(3, 5, 8:9, 12:16), randomization = FALSE)
make.table('cauchy', seq(0, 1, length.out = 5), c(3, 5, 8:9, 12:16), randomization = FALSE)
make.table('levy', seq(0, 1, length.out = 5), c(3, 5, 8:9, 12:16), randomization = FALSE)
make.table('laplace', seq(0, 1, length.out = 5), c(3, 6, 8:9, 12:16), randomization = FALSE)
make.table('logcauchy', seq(0, 1, length.out = 5), c(3, 5, 8:9, 12:13, 15, 17:18), randomization = FALSE)

make.table('norm', seq(0, 1, length.out = 5), c(3, 5, 7:9, 11:15), randomization = TRUE)
make.table('cauchy', seq(0, 1, length.out = 5), c(3, 5, 7:8, 11:15), randomization = TRUE)
make.table('levy', seq(0, 1, length.out = 5), c(3, 5, 7:8, 11:15), randomization = TRUE)
make.table('laplace', seq(0, 1, length.out = 5), c(3, 5, 7:8, 11:15), randomization = TRUE)
make.table('logcauchy', seq(0, 1, length.out = 5), c(3, 5, 7:8, 11:14, 16:17), randomization = TRUE)

make.table('cauchy', seq(0, 0.4, length.out = 5), c(3, 6, 5, 8:9, 12:16), randomization = FALSE, n = 200)

table.name <- make.table.name("significancy", n = 5, M = 1000, D = 0, randomization = FALSE)
cat('% ', file = table.name)
for (distr in DISTRIBUTIONS) {
  res <- rbind(c(paste("rand", F2(distr, c(0,1))), read.res(distr, 'mean', c(0, 1), 5, 1000, 0, randomization = TRUE)),
               c(F2(distr, c(0,1)), read.res(distr, 'mean', c(0, 1), 5, 1000, 0, randomization = FALSE)))
  cat('% ', file = table.name, append = TRUE)
  write.table(res[, 1 + c(0, 3, 5, 7:8, 11:13)], table.name, 
              quote = F, sep = ' & ', eol = ' \\\\\n',  row.names = F, col.names = T, append = TRUE)
  write('\\hline', table.name, append = TRUE)
}

res <- read.res('cauchy', 'mean', cbind(0, 1), randomization = FALSE, prefix = 'L2LLc', n = 1000)
write.row('cauchy', 'mean', cbind(0, 1), randomization = FALSE, prefix = 'L2LLc', n = 1000, tests.numbers = 1:4, col.names = TRUE)
for (par2 in seq(0.05, 0.2, length.out = 4)) {
  write.row('cauchy', 'mean', c(par2, 1), randomization = FALSE, prefix = 'L2LLc', n = 1000, tests.numbers = 1:4)
}

write.table(t(c(50, 'C(1,1)', read.res('cauchy', 'mean', c(1,1), n=50, randomization = FALSE)[c(5, 12, 15:16)]*100)), 'tables/L2LLc,M=1000,D=800.tex', 
            quote = F, sep = ' & ', eol = ' \\\\\n', row.names = F, col.names = F, append = FALSE)
write.table(t(c(200, 'C(0.5,1)', read.res('cauchy', 'mean', c(0.5,1), n=200, randomization = FALSE, prefix = 'L2LLc')[1:4]*100)), 'tables/L2LLc,M=1000,D=800.tex', 
            quote = F, sep = ' & ', eol = ' \\\\\n', row.names = F, col.names = F, append = TRUE)
write.table(t(c(1250, 'C(0.2,1)', read.res('cauchy', 'mean', c(0.2,1), n=1250, randomization = FALSE, prefix = 'L2LLc')[1:4]*100)), 'tables/L2LLc,M=1000,D=800.tex', 
            quote = F, sep = ' & ', eol = ' \\\\\n', row.names = F, col.names = F, append = TRUE)
write.table(t(c(5000, 'C(0.1,1)', read.res('cauchy', 'mean', c(0.1,1), n=5000, randomization = FALSE, prefix = 'L2LLc')[1:4]*100)), 'tables/L2LLc,M=1000,D=800.tex', 
            quote = F, sep = ' & ', eol = ' \\\\\n', row.names = F, col.names = F, append = TRUE)

nnn <- c(3, 5, 8, 9, 12:14, 15, 17, 18)
par <- c(1, 1)
res <- read.res('logcauchy', 'mean', par, randomization = FALSE)[nnn]*100
res
write.table(t(res), 'temp', 
            quote = F, sep = ' & ', eol = ' \\\\\n',
            row.names = F, col.names = F, append = FALSE)
