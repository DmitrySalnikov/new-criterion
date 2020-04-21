source('/home/d/1/new_criteria/funcs/funcs.R')

short.names <- c(cauchy = 'C', norm = 'N', levy = 'Le', laplace = 'La', logcauchy = 'LC')
F2 <- function(distribution, par2) {
  paste0(short.names[distribution], '(', par2[1], ', ', par2[2], ')')
}

make.table.name <- function(distribution, n, M, D) {
  paste0(path, '/tables/', distribution, ',n=', n, ',M=', M, ',D=', D, '.tex')
}

write.row <- function(distribution, type, par2, tests.numbers, n = 50, M = 1000, D = 800, col.names = FALSE) {
  res <- c(F2(distribution, par2),
           round(read.res(distribution, type, par2, n, M, D)[tests.numbers], 3) * 100)
  table.name <- make.table.name(distribution, n, M, D)
  if (col.names) {
    cat('% ', file = table.name)
  }
  write.table(t(res), table.name, 
              quote = F, sep = ' & ', eol = ' \\\\\n',
              row.names = F, col.names = col.names, append = TRUE)
}

make.table <- function(distribution, par, tests.numbers, n = 50, M = 1000, D = 800) {
  table.name <- make.table.name(distribution, n, M, D)
  write.row(distribution, 'mean', c(0, 1), tests.numbers, n, M, D, TRUE)
  write('\\hline', table.name, append = TRUE)
  for (x in par[-1]) {
    write.row(distribution, 'mean', c(x, 1), tests.numbers, n, M, D)
  }
  write('\\hline', table.name, append = TRUE)
  for (x in par[-1]) {
    write.row(distribution, 'var', c(0, 1 + 2 * x), tests.numbers, n, M,  D)
  }
  write('\\hline', table.name, append = TRUE)
  for (x in par[-1]) {
    write.row(distribution, 'mean1var', c(x, 1 + x), tests.numbers, n, M, D)
  }
  write('\\hline', table.name, append = TRUE)
  for (x in par[-1]) {
    write.row(distribution, 'mean2var', c(x, 1 + 2 * x), tests.numbers, n, M, D)
  }  
  write('\\hline', table.name, append = TRUE)
}

read.res('laplace', 'mean', cbind(1, 1), M = 10000, D = 1600)
read.res('logcauchy', 'mean', cbind(1, 1))
read.res('norm', 'mean', cbind(1, 1))

make.table('norm', seq(0, 1, length.out = 5), c(3, 5, 7:9, 11:15))
make.table('cauchy', seq(0, 1, length.out = 5), c(3, 5, 7:8, 11:15))
make.table('levy', seq(0, 1, length.out = 5), c(3, 5, 7:8, 11:15))
make.table('laplace', seq(0, 1, length.out = 5), c(3, 5, 7:8, 11:15), M = 10000, D = 1600)
make.table('logcauchy', seq(0, 1, length.out = 5), c(3, 5, 7:8, 11:14, 16:17))
