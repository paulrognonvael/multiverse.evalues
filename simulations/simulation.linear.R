library(mvtnorm)
setwd("~/GitHub/multiverse.evalues/")
source('routines.R')
set.seed(35)

p.sim=20
beta = c(0.05,0.1,0.2,0.4,0.5,0.75,1,rep(0,p.sim-7))

cov = matrix(rep(0.5,p.sim*p.sim), ncol=p.sim)
diag(cov) = rep(1,p.sim)

nb.sim = 30
summary.mixt.res = data.frame()
summary.soft.res = data.frame()
summary.res.bf = data.frame()

for(n.sim in round(exp(seq(log(100), log(10000), length.out = 10)))){
  cat('\n------- n =',n.sim,'--------\n')
  softrankevalues.res= data.frame()
  mixtevalues.res= data.frame()
  bf.res = data.frame()
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
    
    # construct formula
    my.formula.string = paste('y', "~", paste(colnames(x), collapse = " + "))
    my.formula= as.formula(my.formula.string)
    
    # compute evalues, BF
    supp.stats = hypsupp(formula=my.formula, data=datareg, family='normal', 
                                softrank=TRUE, mixtevalue=FALSE, BF=FALSE)$stats
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
  write.csv(softrankevalues.res,paste0('simulations/linreg/logsoftevalues.sim.n',n.sim,'.csv'),row.names = FALSE)
  summary.soft.sim = as.matrix(t(colMeans(softrankevalues.res)))
  summary.soft.res = rbind(summary.soft.res, summary.soft.sim)
  
  mixtevalues.res$sim = as.numeric(rownames(mixtevalues.res))
  mixtevalues.res$n = rep(n.sim, nrow(mixtevalues.res))
  write.csv(mixtevalues.res,paste0('simulations/linreg/logmixtevalues.sim.n',n.sim,'.csv'),row.names = FALSE)
  summary.mixt.sim = as.matrix(t(colMeans(mixtevalues.res)))
  summary.mixt.res = rbind(summary.mixt.res, summary.mixt.sim)
  
  bf.res$sim = as.numeric(rownames(bf.res))
  bf.res$n = rep(n.sim, nrow(bf.res))
  write.csv(bf.res,paste0('simulations/linreg/logbf.sim.n',n.sim,'.csv'),row.names = FALSE)
  summary.sim.bf = as.matrix(t(colMeans(bf.res)))
  summary.res.bf = rbind(summary.res.bf, summary.sim.bf)
}

write.csv(mixtevalues.res,paste0('simulations/linreg/summaries.logmixt.sim.csv'),row.names = FALSE)
write.csv(softrankevalues.res,paste0('simulations/linreg/summaries.logsoft.sim.csv'),row.names = FALSE)
write.csv(summary.res.bf,paste0('simulations/linreg/summaries.logsim.bf.csv'),row.names = FALSE)



