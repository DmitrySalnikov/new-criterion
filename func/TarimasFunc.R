library(rmutil)

loglik <- function(par, x, dist) {
  ret = NA
  if(dist =="cauchy") ret = -sum(dcauchy(x,
                                         location = par[1], scale = par[2], log = TRUE))
  if(dist =="normal") ret = -sum(dnorm(x,
                                       mean = par[1], sd = par[2], log = TRUE))
  if(dist =="laplace") ret = -sum(dlaplace(x,
                                           m = par[1], s = par[2], log = TRUE))
  ret
}

rdist <- function(n, l, s, dist) {
  ret = NA
  if(dist =="cauchy")  ret = rcauchy( n=n, location = l, scale = s)
  if(dist =="normal")  ret = rnorm(   n=n, mean     = l, sd = s)
  if(dist =="laplace") ret = rlaplace(n=n, m        = l, s = s)
  ret
}

mle <- function(x, dist, n_st = 3) {
  ret=NA
  if(dist != "normal"){
    out <- data.frame(par1_st = rep(NA,n_st),
                      par2_st = rep(NA,n_st),
                      par1 = rep(NA,n_st),
                      par = rep(NA,n_st),
                      val = rep(NA,n_st))
    for(s in 1:n_st){
      out[s,1:2] <- c(runif(1)*6-3,runif(1)*2+0.5)
      tmp <- optim(c(out[s,1],out[s,2]), loglik,
                   method = "L-BFGS-B", lower = c(-3,0.5), upper = c(3,2),
                   x=x, dist = dist)
      out[s,3:5] <- c(tmp[[1]],tmp[[2]])
    }
    ret = out[out$val == min(out$val),3:4]
  }
  if(dist == "normal") ret=data.frame(par1=mean(x),par=sd(x))
  data.frame(ret)
}