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

MakePlot <- function(idx, main = "N(0, 1) vs N(x, 1)", location = 'topleft') {
  tests <- c('K1', 'K4', 'K6', 'K7', 'K71', 'K8', 'K81', 'K9', 'K91', 'K10', 't.test', 'wilcox', 'ks.test', 'var.test')
  col <- 1:length(tests)
  col[3:length(col)]=col[3:length(col)]-1
  col[4] <- 'darkgreen'
  
  pch <- 1:length(tests)
  pch[3:length(pch)]=pch[3:length(pch)]-1
  
  pdf(paste0(plots, pname, '.pdf'), 
      width = 7, height = 7, paper = "special")
  matplot(row.names(res), res[,idx], type = "b", ylab = "Power", xlab = "x", 
          main = main, lty = 4, pch = pch[idx-2], col = col[idx-2], ylim = c(0, 1), 
          lwd = 3, cex = 2, cex.axis = 1.8, cex.main = 2, cex.lab = 1.5, font.lab = 2)
  legend(bty = 'n', location, legend = tests[idx-2], lty = 4, pch = pch[idx-2], 
         col = col[idx-2], lwd = 3, cex = 2)
  dev.off()
}

# norm mean
{
ress <- '/home/d/Study/publication2019/res/norm/mean/'
tables <- '/home/d/Study/publication2019/tables/norm/mean/'
plots <- '/home/d/Study/publication2019/plots/norm/mean/'

for (resName in dir(ress)) {
  rname <- substr(resName,1,nchar(resName)-4)
  res <- readRDS(paste0(ress, resName))
  print(rname)
  print(res)
  
  c1 <- c(paste0('$N(',row.names(res)[1],',1)$'), rep('',dim(res)[1]-1))
  c2 <- paste0('$N(',row.names(res),',1)$')
  res <- round(res, 3)
  res <- cbind(c2, res)
  res <- cbind(c1, res)
  
  tname <- rname
  MakeTable(3:12,13:15)
  
  tname = paste0(rname,'_OldTests')
  MakeTable(c(3,5,12),13:15)
  
  tname = paste0(rname,'_NewTests')
  MakeTable(6:11)
}
}

# cauchy mean
{
  ress <- '/home/d/Study/publication2019/res/cauchy/mean/'
  tables <- '/home/d/Study/publication2019/tables/cauchy/mean/'
  plots <- '/home/d/Study/publication2019/plots/cauchy/mean/'
  
  for (resName in dir(ress)) {
    rname <- substr(resName,1,nchar(resName)-4)
    res <- readRDS(paste0(ress, resName))
    print(rname)
    print(res)
    
    c1 <- c(paste0('$C(',row.names(res)[1],',1)$'), rep('',dim(res)[1]-1))
    c2 <- paste0('$C(',row.names(res),',1)$')
    res <- round(res, 3)
    res <- cbind(c2, res)
    res <- cbind(c1, res)
    
    tname <- rname
    MakeTable(3:12,13:15)
    
    tname = paste0(rname,'_OldTests')
    MakeTable(c(3,5,12),13:15)
    
    tname = paste0(rname,'_NewTests')
    MakeTable(6:11)
  }
}
