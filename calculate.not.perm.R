source('/home/d/1/new_criteria/funcs/Power.not.perm.R')

Power.cauchy.not.perm('mean', c(0,1), n=50, K=10000)
Power.cauchy.not.perm('mean', c(1,1), n=50, K=10000)
Power.cauchy.not.perm('mean', c(0.5,1), n=200, K=10000)
Power.cauchy.not.perm('mean', c(0.2,1), n=1250, K=10000)
Power.cauchy.not.perm('mean', c(0.1,1), n=5000, K=10000)