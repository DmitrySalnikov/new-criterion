readRDS.type <- function(distribution, n, percents, prefix, type) {
  if (type != 'mean' && type != 'var' && type != 'mean1var' && type != 'mean2var') {
    stop("\nSelect type from:\n  1)  mean;\n  2)  var;\n  3)  mean1var;\n  4)  mean2var.")
  }
  
  size <- if (n == 5) {
    'N5M1000Exact.RDS'
  } else {
    'N50M1000D800.RDS'
  } 
  res <- round(readRDS(paste0('res/', distribution, '/', type, size)), 3)
  if (percents) {
    res <- res * 100
  }
  
  switch (type,
    mean = {
      first <- rownames(res)
      second <- 1
    }, var = {
      first <- 0
      second <- rownames(res)
    }, mean1var = {
      first <- rownames(res)
      second <- 1 + as.numeric(rownames(res))
    }, mean2var = {
      first <- rownames(res)
      second <- 1 + 2*as.numeric(rownames(res))
    }
  )
  c2 <- c(paste0(prefix, '(', first, ', ', second, ')'))
  c1 <- rep(c2[1], 5)
  cbind(c1, c2, res)
}

readRDS.distribution <- function(distribution, n = 50, percents = TRUE, prefix = NULL) {
  if (is.null(prefix)) {
    prefix <- toupper(if (substr(distribution, 1, 3) == 'log') {
      paste0('L', substr(distribution, 4, 4))
    } else {
      substr(distribution, 1, 1)
    })
  }
  
  res <- rbind(
    readRDS.type(distribution, n, percents, prefix, 'mean'),
    readRDS.type(distribution, n, percents, prefix, 'var'),
    readRDS.type(distribution, n, percents, prefix, 'mean1var'),
    readRDS.type(distribution, n, percents, prefix, 'mean2var')
  )
  rownames(res) <- 1:length(rownames(res))
  res
}

MakeTable <- function(res, file = 'table.tex', with_F1 = TRUE, append = FALSE) {
  if (!with_F1) res <- res[,-1]
  
  cat('% ', file = file, append = append)
  write.table(res, file,
    quote = F, sep = ' & ', eol = ' \\\\\n',
    row.names = F, col.names = T, append = TRUE)
  write('\\hline', file, append = TRUE)
}

res.norm <- readRDS.distribution('norm')[, -c(4, 6, 8)]
res.norm
res.norm <- res.norm[c(1, 4, 8, 13, 18), ]
MakeTable(res.norm, 'table2.tex')

res.cauchy <- readRDS.distribution('cauchy')[, -c(4, 6, 8)]
res.cauchy
res.cauchy <- res.cauchy[c(1, 3, 8, 13, 18), ]
MakeTable(res.cauchy, 'table2.tex', append = TRUE)

res.levy <- readRDS.distribution('levy')[, -c(4, 6, 8)]
res.levy
res.levy <- res.levy[c(1, 5, 10, 13, 18), ]
MakeTable(res.levy, 'table2.tex', append = TRUE)


res.logcauchy <- readRDS.distribution('logcauchy')[, -c(4, 6, 8)]
res.logcauchy
res.logcauchy <- res.logcauchy[c(1, 5, 8, 15, 20), -c(6, 8)]

MakeTable(res.logcauchy, 'table3.tex')
