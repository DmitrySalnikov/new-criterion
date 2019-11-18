source('/home/d/Study/publication2019/func/funcs.R')
library('EnvStats')
# params
alpha <- 0.05
M <- 2500
n <- 5
N = 2*n

RDSpath <- '/home/d/Study/publication2019/thesis/helpers/RDS5/'
file_path <- '/home/d/Study/publication2019/thesis/helpers/tables/res5'

func <- function(res, RDSname, distr_names) {
  print(res)
  saveRDS(c(distr_names,res),paste0(RDSpath,RDSname,'.RDS'))
  res <- round(res,3)
  res <- c(distr_names,res)
  write.table(t(res), file_path, 
              quote = F, sep = ' & ', row.names = F, eol = '\n', 
              col.names = F, append = TRUE)  
}

func(Power(t(replicate(M, 
                       c(rnorm(n,0,1), rnorm(n,2,1)))), exact = TRUE), 
     'norm01_21', c('$N(0,1)$','$N(2,1)$'))
func(Power(t(replicate(M, 
                       c(rnorm(n,0,1), rnorm(n,0,9)))), exact = TRUE), 
     'norm01_09', c('$N(0,1)$','$N(0,9)$'))
func(Power(t(replicate(M, 
                       c(rnorm(n,0,1), rnorm(n,4,4)))), exact = TRUE), 
     'norm01_44', c('$N(0,1)$','$N(4,4)$'))
func(Power(t(replicate(M, 
                       c(rcauchy(n,0,1), rcauchy(n,5,1)))), exact = TRUE), 
     'cauchy01_51', c('$C(0,1)$','$C(5,1)$'))
func(Power(t(replicate(M, 
                       c(rcauchy(n,0,1), rcauchy(n,0,20)))), exact = TRUE), 
     'cauchy01_020', c('$C(0,1)$','$C(0,20)$'))
func(Power(t(replicate(M, 
                       c(rlnorm(n,0,1), rlnorm(n,2,1)))), exact = TRUE), 
     'lnorm01_21', c('$LN(0,1)$','$LN(2,1)$'))
func(Power(t(replicate(M, 
                       c(rlnorm(n,0,1), rlnorm(n,0,40)))), exact = TRUE), 
     'lnorm01_040', c('$LN(0,1)$','$LN(0,40)$'))
func(Power(t(replicate(M, 
                       c(rpareto(n,1,1), rpareto(n,5,1)))), exact = TRUE), 
     'pareto11_51', c('$P(1,1)$','$P(5,1)$'))
func(Power(t(replicate(M, 
                       c(rpareto(n,1,2), rpareto(n,1,20)))), exact = TRUE), 
     'pareto12_120', c('$P(1,2)$','$P(1,20)$'))
func(Power(t(replicate(M, 
                       c(rweibull(n,2,2), rweibull(n,2,6)))), exact = TRUE), 
     'weibull22_26', c('$W(2,2)$','$W(2,6)$'))
func(Power(t(replicate(M, 
                       c(rweibull(n,2,2), rweibull(n,20,2)))), exact = TRUE), 
     'weibull22_202', c('$W(2,2)$','$W(20,2)$'))
func(Power(t(replicate(M, 
                       c(rgamma(n,3,scale = 1), rgamma(n,3,scale = 5)))), exact = TRUE), 
     'gamma31_35', c('$G(3,1)$','$G(3,5)$'))
func(Power(t(replicate(M, 
                       c(rgamma(n,1,scale = 1), rgamma(n,5,scale = 1)))), exact = TRUE), 
     'gamma11_51', c('$G(1,1)$','$G(5,1)$'))
func(Power(t(replicate(M, 
                       c(rbeta(n,2,2), rbeta(n,9,2)))), exact = TRUE), 
     'beta22_92', c('$B(2,2)$','$B(9,2)$'))
func(Power(t(replicate(M, 
                       c(rbeta(n,1,1), rbeta(n,40,40)))), exact = TRUE), 
     'beta11_4040', c('$B(1,1)$','$B(40,40)$'))
