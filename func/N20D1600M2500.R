source('/home/d/Study/publication2019/func/funcs.R')
library('EnvStats')
# params
alpha <- 0.05
M <- 2500
D <- 1600
n <- 20
N = 2*n

RDSpath <- '/home/d/Study/publication2019/thesis/helpers/RDS/'
file_path <- '/home/d/Study/publication2019/thesis/helpers/res20'

func <- function(res, RDSname, distr_names) {
  saveRDS(c(distr_names,res),paste0(RDSpath,RDSname,'.RDS'))
  res <- round(res,3)
  res <- c(distr_names,res)
  write.table(t(res), file_path, 
              quote = F, sep = ' & ', row.names = F, eol = '\n', 
              col.names = F, append = TRUE)  
}

func(Power(t(replicate(M, 
                       c(rnorm(n,0,1), rnorm(n,1,1))))), 
    'norm01_11', c('$N(0,1)$','$N(1,1)$'))
func(Power(t(replicate(M, 
                       c(rnorm(n,0,1), rnorm(n,0,3))))), 
     'norm01_03', c('$N(0,1)$','$N(0,3)$'))
func(Power(t(replicate(M, 
                       c(rnorm(n,0,1), rnorm(n,1,2))))), 
     'norm01_12', c('$N(0,1)$','$N(1,2)$'))
func(Power(t(replicate(M, 
                       c(rcauchy(n,0,1), rcauchy(n,2,1))))), 
     'cauchy01_21', c('$C(0,1)$','$C(2,1)$'))
func(Power(t(replicate(M, 
                       c(rcauchy(n,0,1), rcauchy(n,0,6))))), 
     'cauchy01_06', c('$C(0,1)$','$C(0,6)$'))
func(Power(t(replicate(M, 
                       c(rlnorm(n,0,1), rlnorm(n,1,1))))), 
     'lnorm01_11', c('$LN(0,1)$','$LN(1,1)$'))
func(Power(t(replicate(M, 
                       c(rlnorm(n,0,1), rlnorm(n,0,4))))), 
     'lnorm01_04', c('$LN(0,1)$','$LN(0,4)$'))
func(Power(t(replicate(M, 
                       c(rpareto(n,1,1), rpareto(n,3,1))))), 
     'pareto11_31', c('$P(1,1)$','$P(3,1)$'))
func(Power(t(replicate(M, 
                       c(rpareto(n,1,2), rpareto(n,1,6))))),
     'pareto11_31', c('$P(1,1)$','$P(3,1)$'))
func(Power(t(replicate(M, 
                       c(rf(n,40,2), rf(n,40,20))))), 
     'fisher402_4020', c('$F(40,2)$','$F(40,20)$'))
func(Power(t(replicate(M, 
                       c(rf(n,2,40), rf(n,20,40))))), 
     'fisher240_2040', c('$F(2,40)$','$F(20,40)$'))
func(Power(t(replicate(M, 
                       c(rweibull(n,2,2), rweibull(n,2,4))))), 
     'weibull22_24', c('$W(2,2)$','$W(2,4)$'))
func(Power(t(replicate(M, 
                       c(rweibull(n,2,2), rweibull(n,8,2))))), 
     'weibull22_82', c('$W(2,2)$','$W(8,2)$'))
func(Power(t(replicate(M, 
                       c(rgamma(n,3,scale = 1), rgamma(n,3,scale = 2))))), 
     'gamma31_32', c('$G(3,1)$','$G(3,2)$'))
func(Power(t(replicate(M, 
                       c(rgamma(n,1,scale = 1), rgamma(n,2,scale = 1))))), 
     'gamma11_21', c('$G(1,1)$','$G(2,1)$'))
func(Power(t(replicate(M, 
                       c(rbeta(n,2,2), rbeta(n,5,2))))), 
     'beta22_52', c('$B(2,2)$','$B(5,2)$'))
func(Power(t(replicate(M, 
                       c(rbeta(n,1,1), rbeta(n,8,8))))), 
     'beta11_88', c('$B(1,1)$','$B(8,8)$'))
