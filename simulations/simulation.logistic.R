library(mvtnorm)
setwd("~/GitHub/multiverse.evalues/")
source('routines.R')
set.seed(35)

sim.logistic = function(n,p,beta,cov){
  x = rmvnorm(n, mean=rep(0,p),cov)
  x <- x %*% diag(1/sqrt(diag(t(x) %*% x)/n))
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

p.sim=10
beta = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.5, 0.75, 1)
cov = matrix(rep(0.5,p.sim*p.sim), ncol=p.sim)
diag(cov) = rep(1,p.sim)
nb.sim = 100
summary.mixt.res = data.frame()
summary.soft.res = data.frame()
summary.bf.res = data.frame()
summary.p.values.res = data.frame()
summary.calib1.res = data.frame()
summary.calib2.res = data.frame()
summary.calib3.res = data.frame()
summary.calib4.res = data.frame()

for(n.sim in c(100, 250, 500, 750, 1000, 1500, 2000, 2500, 5000, 10000)){
  cat('\n------- n =',n.sim,'--------\n')
  softrankevalues.res= data.frame()
  mixtevalues.res= data.frame()
  bf.res = data.frame()
  p.values.res = data.frame()
  calib1.res = data.frame()
  calib2.res = data.frame()
  calib3.res = data.frame()
  calib4.res = data.frame()
  
  for(i in 1:nb.sim){
    cat('\n------------- simulation',i,'--------------\n')
    # simulate data
    datareg = sim.logistic (n=n.sim,p=p.sim, beta, cov)
    
    # construct formula
    my.formula.string = paste('y', "~", paste(colnames(datareg[,-1]), collapse = " + "))
    my.formula= as.formula(my.formula.string)
    
    # compute evalues, BF
    supp.stats = hypsupp(formula=my.formula, data=datareg, family='binomial', 
                         softrank=TRUE, mixtevalue=TRUE, BF=FALSE, p.to.e = TRUE)$stats
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
    
    # # store BF
    # bf.df = matrix(ncol=length(supp.stats$var))
    # colnames(bf.df) = supp.stats$var
    # bf.df[1,] = supp.stats$logBF
    # bf.res = rbind(bf.res,bf.df)
    
    # store p.values
    p.values.df = matrix(ncol=length(supp.stats$var))
    colnames(p.values.df) = supp.stats$var
    p.values.df[1,] = supp.stats$anov.pvalue
    p.values.res = rbind(p.values.res,p.values.df)
    
    # store calibrated e-values (calibrator 1)
    calib1.df = matrix(ncol=length(supp.stats$var))
    colnames(calib1.df) = supp.stats$var
    calib1.df[1,] = supp.stats$logcalib1
    calib1.res = rbind(calib1.res,calib1.df)
    
    # store calibrated e-values (calibrator 2)
    calib2.df = matrix(ncol=length(supp.stats$var))
    colnames(calib2.df) = supp.stats$var
    calib2.df[1,] = supp.stats$logcalib2
    calib2.res = rbind(calib2.res,calib2.df)
    
    # store calibrated e-values (calibrator 3)
    calib3.df = matrix(ncol=length(supp.stats$var))
    colnames(calib3.df) = supp.stats$var
    calib3.df[1,] = supp.stats$logcalib3
    calib3.res = rbind(calib3.res,calib3.df)
    
    # store calibrated e-values (calibrator 4)
    calib4.df = matrix(ncol=length(supp.stats$var))
    colnames(calib4.df) = supp.stats$var
    calib4.df[1,] = supp.stats$logcalib4
    calib4.res = rbind(calib4.res,calib4.df)
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
  
  # bf.res$sim = as.numeric(rownames(bf.res))
  # bf.res$n = rep(n.sim, nrow(bf.res))
  # write.csv(bf.res,paste0('simulations/logreg/logbf.sim.n',n.sim,'.csv'),row.names = FALSE)
  # summary.sim.bf = as.matrix(t(colMeans(bf.res)))
  # summary.bf.res = rbind(summary.bf.res, summary.sim.bf)
  
  p.values.res$sim = as.numeric(rownames(p.values.res))
  p.values.res$n = rep(n.sim, nrow(p.values.res))
  write.csv(p.values.res,paste0('simulations/logreg/p.values.sim.n',n.sim,'.csv'),row.names = FALSE)
  summary.sim.p.values = as.matrix(t(colMeans(p.values.res)))
  summary.p.values.res = rbind(summary.p.values.res, summary.sim.p.values)
  
  calib1.res$sim = as.numeric(rownames(calib1.res))
  calib1.res$n = rep(n.sim, nrow(calib1.res))
  write.csv(calib1.res,paste0('simulations/logreg/logcalib1.sim.n',n.sim,'.csv'),row.names = FALSE)
  summary.sim.calib1 = as.matrix(t(colMeans(calib1.res)))
  summary.calib1.res = rbind(summary.calib1.res, summary.sim.calib1)
  
  calib2.res$sim = as.numeric(rownames(calib2.res))
  calib2.res$n = rep(n.sim, nrow(calib2.res))
  write.csv(calib2.res,paste0('simulations/logreg/logcalib2.sim.n',n.sim,'.csv'),row.names = FALSE)
  summary.sim.calib2 = as.matrix(t(colMeans(calib2.res)))
  summary.calib2.res = rbind(summary.calib2.res, summary.sim.calib2)
  
  calib3.res$sim = as.numeric(rownames(calib3.res))
  calib3.res$n = rep(n.sim, nrow(calib3.res))
  write.csv(calib3.res,paste0('simulations/logreg/logcalib3.sim.n',n.sim,'.csv'),row.names = FALSE)
  summary.sim.calib3 = as.matrix(t(colMeans(calib3.res)))
  summary.calib3.res = rbind(summary.calib3.res, summary.sim.calib3)
  
  calib4.res$sim = as.numeric(rownames(calib4.res))
  calib4.res$n = rep(n.sim, nrow(calib4.res))
  write.csv(calib4.res,paste0('simulations/logreg/logcalib4.sim.n',n.sim,'.csv'),row.names = FALSE)
  summary.sim.calib4 = as.matrix(t(colMeans(calib4.res)))
  summary.calib4.res = rbind(summary.calib4.res, summary.sim.calib4)
}

write.csv(summary.mixt.res,paste0('simulations/logreg/summaries.logmixt.sim.csv'),row.names = FALSE)
write.csv(summary.soft.res,paste0('simulations/logreg/summaries.logsoft.sim.csv'),row.names = FALSE)
#write.csv(summary.bf.res,paste0('simulations/logreg/summaries.logbf.sim.csv'),row.names = FALSE)
write.csv(summary.p.values.res,paste0('simulations/logreg/summaries.pvalues.sim.csv'),row.names = FALSE)
write.csv(summary.calib1.res,paste0('simulations/logreg/summaries.logcalib1.sim.csv'),row.names = FALSE)
write.csv(summary.calib2.res,paste0('simulations/logreg/summaries.logcalib2.sim.csv'),row.names = FALSE)
write.csv(summary.calib3.res,paste0('simulations/logreg/summaries.logcalib3.sim.csv'),row.names = FALSE)
write.csv(summary.calib4.res,paste0('simulations/logreg/summaries.logcalib4.sim.csv'),row.names = FALSE)


summary.mixt.res$type='mixt'
summary.soft.res$type='soft'
#summary.bf.res$type='bf'
summary.p.values.res$type='pvalues'
summary.calib1.res$type='calib1'
summary.calib2.res$type='calib2'
summary.calib3.res$type='calib3'
summary.calib4.res$type='calib4'
summary.all.res=rbind(summary.mixt.res,
                      summary.soft.res,
                      #summary.bf.res,
                      summary.p.values.res,
                      summary.calib1.res,
                      summary.calib2.res,
                      summary.calib3.res,
                      summary.calib4.res)
write.csv(summary.all.res,'~/GitHub/multiverse.evalues/simulations/logreg/summaries.all.csv',row.names = FALSE)
