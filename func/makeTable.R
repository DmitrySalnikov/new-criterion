MakeTable <- function(idx1,idx2 = vector()) {
  if (!length(idx2))
    write.table(data.frame("$F_1$", "$F_2$", 
                           t(paste0('$\\K_{', substr(colnames(res)[idx1],2,10), '}$'))
    ), paste0(tables, tname), quote = F, sep = ' & ', row.names = F, 
    eol = ' \\\\\ \\hline\n', col.names = F)
  else
    write.table(data.frame("$F_1$", "$F_2$", 
                           t(paste0('$\\K_{', substr(colnames(res)[idx1],2,10), '}$')),
                           t(colnames(res)[idx2])
    ), paste0(tables, tname), quote = F, sep = ' & ', row.names = F, 
    eol = ' \\\\\ \\hline\n', col.names = F)
  write.table(res[,c(1:2,idx1,idx2)], paste0(tables, tname), 
              quote = F, sep = ' & ', row.names = F, eol = ' \\\\\n', 
              col.names = F, append = TRUE)
  write('\\hline', paste0(tables, tname), append = T)
}

# params

ress <- '/home/d/Study/new_criteria/res/norm/'
tables <- '/home/d/Study/new_criteria/tables/norm/'

n <- 5
M <- 1024
D <- 1600
prefix <- 'N'
first <- vector()
second <- vector()
par <- seq(0, 0.4, length.out = 5)
res <- readRDS('../res/norm/NormMeanVar2X0ExactN100M1000D800.rds')
for (i in par[-1]*10) res <- rbind(res, readRDS(paste0('../res/norm/NormMeanVar2X0_', i, 'ExactN100M1000D800.rds')))
first <- par
second <- 1+par
c2 <- paste0('$',prefix,'(',first,', ',second,')$')
c1 <- c(c2[1], rep('',4))
res <- round(res, 3)
res <- cbind(c2, res)
res <- cbind(c1, res)
res
tname <- 'NormMeanVar2ExactN100M1000D800'

MakeTable(3:6,12:16)
