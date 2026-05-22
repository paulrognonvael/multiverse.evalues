library(mvtnorm)
setwd("~/GitHub/multiverse.evalues/")
source('routines.R')
set.seed(35)
# n=50
# p=20
# x =rmvnorm(n, mean=rep(0,p))
# beta = c(1/2,2/3,1:5,rep(0,13))
# 
# 
# #### Linear regression 
# y = x %*% beta + rnorm(n,sd=sqrt(0.5))
# datareg = data.frame(y=y)
# datareg = cbind(datareg, x)
# 
# 
# res.split = evalues.split(formula=y~., data=datareg, family='normal')
# res.split$evalues
# 
# res.nosplitmanual = evalues.nosplit(formula=y~., data=datareg, family='normal')
# res.nosplitmanual$evalues
# 
# res.nosplitbestIC = evalues.nosplit2(formula=y~., data=datareg, family='normal')
# res.nosplitbestIC$evalues
# 
# evalues.df = data.frame(split=res.split$evalues, 
#                         nosplit.manual = res.nosplitmanual$evalues, 
#                         nonsplit.bestIC = res.nosplitbestIC$evalues)
# 
# View(res.nosplitmanual$loglik)
# View(res.nosplitbestIC$loglik)


### Simulation

p.sim=20
beta = c(0.2,0.4,0.6,0.8,1,1.25,1.5,rep(0,p.sim-7))

cov = matrix(rep(0.5,p.sim*p.sim), ncol=p.sim)
diag(cov) = rep(1,p.sim)

nb.sim = 100
summary.res = data.frame()

for(n.sim in round(exp(seq(log(100), log(50000), length.out = 10)))){
  cat('\n------- n =',n.sim,'--------\n')
  evalues.res = data.frame()
  for(i in 1:nb.sim){
    cat('\n------------- simulation',i,'--------------\n')
    x =rmvnorm(n.sim, mean=rep(0,p.sim), cov)
    x = x %*% diag(1/sqrt(diag(t(x) %*% x)/n.sim))
    if(p.sim<=26){
      colnames(x) = LETTERS[1:p.sim]
    }else{
      colnames(x) = sprintf('X%s',1:p.sim)
    }
    # simulate data
    y = x %*% beta + rnorm(n.sim,sd=1)
    datareg = data.frame(y=y)
    datareg = cbind(datareg, x)
    
    # compute evalues
    res.nosplitbestIC = evalues.nosplit2(formula=y~., data=datareg, family='normal')
    # store evalaues
    evalues.df = matrix(ncol=length(res.nosplitbestIC$evalues$var))
    colnames(evalues.df) = res.nosplitbestIC$evalues$var
    evalues.df[1,] = res.nosplitbestIC$evalues$evalue
    evalues.res = rbind(evalues.res,evalues.df)
  }
  evalues.res$sim = as.numeric(rownames(evalues.res))
  evalues.res$n = rep(n.sim, nrow(evalues.res))
  write.csv(evalues.res,paste0('simulations/linreg/evalues.sim.n',n.sim,'.csv'),row.names = FALSE)
  summary.sim = as.matrix(t(colMeans(evalues.res)))
  summary.res = rbind(summary.res, summary.sim)
}

write.csv(summary.res,paste0('simulations/linreg/summaries.sim.csv'),row.names = FALSE)



