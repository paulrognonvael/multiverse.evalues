library(mvtnorm)
setwd("~/GitHub/multiverse.evalues/")
source('routines.R')
set.seed(35)

sim.logistic = function(n,p,beta){
  x = rmvnorm(n, mean=rep(0,p))
  if(p<=26){
    colnames(x) = LETTERS[1:p]
  }else{
    colnames(x) = sprintf('X%s',1:p)
  }
  
  y = rbinom(n,1, 1/(1+exp(-x %*% beta )))
  datareg = data.frame(y=y)
  datareg = cbind(datareg, x)
  return(datareg)
}

p.sim=20
beta = c(0.05,0.1,0.2,0.4,0.6,0.8,1,rep(0,p.sim-7))
nb.sim = 50#100
summary.res = data.frame()
summary.res.bf = data.frame()

for(n.sim in round(exp(seq(log(100), log(30000), length.out = 10)))){
  cat('\n------- n =',n.sim,'--------\n')
  evalues.res = data.frame()
  bf.res = data.frame()
  for(i in 1:nb.sim){
    cat('\n------------- simulation',i,'--------------\n')
    # simulate data
    datareg = sim.logistic (n=n.sim,p=p.sim, beta)
    
    # compute evalues
    res.nosplitbestIC = evalues.nosplit2(formula=y~., data=datareg, family='binomial')
    # store evalues
    evalues.df = matrix(ncol=length(res.nosplitbestIC$evalues$var))
    colnames(evalues.df) = res.nosplitbestIC$evalues$var
    evalues.df[1,] = res.nosplitbestIC$evalues$evalue
    evalues.res = rbind(evalues.res,evalues.df)
    
    # store BF
    bf.df = matrix(ncol=length(res.nosplitbestIC$evalues$var))
    colnames(bf.df) = res.nosplitbestIC$evalues$var
    bf.df[1,] = res.nosplitbestIC$evalues$BF
    bf.res = rbind(bf.res,bf.df)
  }
  evalues.res$sim = as.numeric(rownames(evalues.res))
  evalues.res$n = rep(n.sim, nrow(evalues.res))
  write.csv(evalues.res,paste0('simulations/logreg/evalues.sim.n',n.sim,'.csv'),row.names = FALSE)
  summary.sim = as.matrix(t(colMeans(evalues.res)))
  summary.res = rbind(summary.res, summary.sim)
  
  bf.res$sim = as.numeric(rownames(bf.res))
  bf.res$n = rep(n.sim, nrow(bf.res))
  write.csv(bf.res,paste0('simulations/logreg/bf.sim.n',n.sim,'.csv'),row.names = FALSE)
  summary.sim.bf = as.matrix(t(colMeans(bf.res)))
  summary.res.bf = rbind(summary.res.bf, summary.sim.bf)
  
}

write.csv(summary.res,paste0('simulations/logreg/summaries.sim.csv'),row.names = FALSE)
write.csv(summary.res.bf,paste0('simulations/logreg/summaries.sim.bf.csv'),row.names = FALSE)

