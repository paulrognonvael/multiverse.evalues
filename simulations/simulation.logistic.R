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
beta = c(0.05,0.1,0.2,0.4,0.5,0.75,1,rep(0,p.sim-7))
nb.sim = 3
summary.res = data.frame()
summary.res.bf = data.frame()

for(n.sim in round(exp(seq(log(100), log(10000), length.out = 2)))){
  cat('\n------- n =',n.sim,'--------\n')
  softrankevalues.res= data.frame()
  mixtevalues.res= data.frame()
  bf.res = data.frame()
  for(i in 1:nb.sim){
    cat('\n------------- simulation',i,'--------------\n')
    # simulate data
    datareg = sim.logistic (n=n.sim,p=p.sim, beta)
    
    # construct formula
    my.formula.string = paste('y', "~", paste(colnames(datareg[,-1]), collapse = " + "))
    my.formula= as.formula(my.formula.string)
    
    # compute evalues, BF
    supp.stats = hypsupp(formula=my.formula, data=datareg, family='binomial', 
                         softrank=TRUE, mixtevalue=TRUE, BF=TRUE)$stats
    # store universal mixture evalues
    mixtevalues.df = matrix(ncol=length(supp.stats$var))
    colnames(mixtevalues.df) = supp.stats$var
    mixtevalues.df[1,] = supp.stats$logmixtevalue
    mixtevalues.res = rbind(mixtevalues.res,mixtevalues.df)
    
    # store soft rank evalues
    softrankevalues.df = matrix(ncol=length(supp.stats$var))
    colnames(softrankevalues.df) = supp.stats$var
    softrankevalues.df[1,] = supp.stats$logsoftevalue
    softrankevalues.res = rbind(softrankevalues.res,softrankevalues.df)
    
    # store BF
    bf.df = matrix(ncol=length(supp.stats$var))
    colnames(bf.df) = supp.stats$var
    bf.df[1,] = supp.stats$logBF
    bf.res = rbind(bf.res,bf.df)
  }
  softrankevalues.res$sim = as.numeric(rownames(softrankevalues.res))
  softrankevalues.res$n = rep(n.sim, nrow(softrankevalues.res))
  write.csv(softrankevalues.res,paste0('simulations/logreg/logsoftevalues.sim.n',n.sim,'.csv'),row.names = FALSE)
  summary.soft.sim = as.matrix(t(colMeans(softrankevalues.res)))
  summary.soft.res = rbind(summary.soft.res, summary.soft.sim)
  
  mixtevalues.res$sim = as.numeric(rownames(mixtevalues.res))
  mixtevalues.res$n = rep(n.sim, nrow(mixtevalues.res))
  write.csv(mixtevalues.res,paste0('simulations/logreg/logmixtevalues.sim.n',n.sim,'.csv'),row.names = FALSE)
  summary.mixt.sim = as.matrix(t(colMeans(mixtevalues.res)))
  summary.mixt.res = rbind(summary.mixt.res, summary.mixt.sim)
  
  bf.res$sim = as.numeric(rownames(bf.res))
  bf.res$n = rep(n.sim, nrow(bf.res))
  write.csv(bf.res,paste0('simulations/linreg/logbf.sim.n',n.sim,'.csv'),row.names = FALSE)
  summary.sim.bf = as.matrix(t(colMeans(bf.res)))
  summary.res.bf = rbind(summary.res.bf, summary.sim.bf)
}

write.csv(mixtevalues.res,paste0('simulations/logreg/summaries.logmixt.sim.csv'),row.names = FALSE)
write.csv(softrankevalues.res,paste0('simulations/logreg/summaries.logsoft.sim.csv'),row.names = FALSE)
write.csv(summary.res.bf,paste0('simulations/logreg/summaries.logsim.bf.csv'),row.names = FALSE)

