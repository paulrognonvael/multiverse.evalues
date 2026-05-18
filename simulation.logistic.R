library(mvtnorm)
setwd("~/GitHub/multiverse.evalues/")
source('routines.R')
set.seed(35)

sim.logistic = function(n,p=30){
  x = rmvnorm(n, mean=rep(0,p))
  beta = c(runif(2,1/2,2/3),1:5,rep(0,p-7))
  y = rbinom(n,1, 1/(1+exp(-x %*% beta )))
  datareg = data.frame(y=y)
  datareg = cbind(datareg, x)
  return(datareg)
}

datareg = sim.logistic(n=800,p=20)

res.split = evalues.split(formula=y~., data=datareg, family='binomial')
res.split$evalues

res.nosplitmanual = evalues.nosplit(formula=y~., data=datareg, family='binomial')
res.nosplitmanual$evalues

res.nosplitbestIC = evalues.nosplit2(formula=y~., data=datareg, family='binomial')
res.nosplitbestIC$evalues

evalues.df = data.frame(split=res.split$evalues, 
                        nosplit.manual = res.nosplitmanual$evalues, 
                        nonsplit.bestIC = res.nosplitbestIC$evalues)

### Simulation

n.sim=50
p.sim=20
beta = c(1/2,2/3,1:5,rep(0,p.sim-7))

cov = matrix(rep(0.5,p.sim*p.sim), ncol=p.sim)
diag(cov) = rep(1,p.sim)

nb.sim = 100
summary.res = data.frame()

for(n.sim in c(250,500,750,1000,1250,1500,1750,2000)){
  cat('\n------- n =',n.sim,'--------\n')
  evalues.res = data.frame()
  for(i in 1:nb.sim){
    # simulate data
    datareg = sim.logistic (n=n.sim,p=p.sim)
    # compute evalues
    res.nosplitbestIC = evalues.nosplit2(formula=y~., data=datareg, family='binomial')
    # store evalaues
    evalues.df = matrix(ncol=length(res.nosplitbestIC$evalues$var))
    colnames(evalues.df) = res.nosplitbestIC$evalues$var
    evalues.df[1,] = res.nosplitbestIC$evalues$evalue
    evalues.res = rbind(evalues.res,evalues.df)
  }
  evalues.res$sim = as.numeric(rownames(evalues.res))
  evalues.res$n = rep(n.sim, nrow(evalues.res))
  write.csv(evalues.res,paste0('simulations/logreg/evalues.sim.n',n.sim,'.csv'),row.names = FALSE)
  summary.sim = as.matrix(t(colMeans(evalues.res)))
  summary.res = rbind(summary.res, summary.sim)
}

write.csv(summary.res,paste0('simulations/logreg/summaries.sim.csv'),row.names = FALSE)
