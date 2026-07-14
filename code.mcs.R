### Loading data
setwd("~/GitHub/multiverse.evalues/")
source('routines.R')
mcs = new.env()
load('data/mcs.Rdata', mcs)
set.seed(35)

library(stringr)

attach(mcs)
names(yvars) 
names(cvars)
x_names

################################################################################
#           eBH-corrected universal mixture evalue - mcs data                  #
################################################################################
#### Loading data ####
#### Computing raw and eBH corrected e-values for individual outcomes ####
mcs_ind.evalues = list(); mcs_ind.eBH = list(); mcs_loglik = list()
mcs_all.evalues = data.frame()
hyptotest = c()
x_names =  c("TV", "Electronic_games", "Social_media", "Other_internet", "Own_computer")

for (idy in 1:length(yvars)){
  yvar = yvars[idy]; yname = names(yvars)[idy]
  cat('Analysing outcome:',yname,'\n')
  datareg = na.omit(data[c(yvar, x_vars, cvars)])
  datareg[datareg[cvars[names(cvars)=='Father']]==2,]=0
  names(datareg) = c('y', x_names, names(cvars))
  
  #interaction.terms = unlist(lapply(x_names, function(x) sprintf(paste0(x,':%s'),names(cvars))))
  my.formula.string = paste('y', "~", paste(c(x_names,names(cvars)), collapse = " + "))
  my.formula= as.formula(my.formula.string)
  
  ###### computing raw universal mixture evalue
  supp = hypsupp(formula=my.formula, data=datareg, family='binomial', vars=x_names, 
                 softrank=TRUE, mixtevalue=TRUE, BF=TRUE, p.to.e=TRUE)
  res = supp$stats[,c('var','logcalib1','logcalib2','logmixtevalue','logsoftevalue','anov.pvalue')]
  res['yvar'] = yname
  res['hyp'] = sprintf(paste0('%sX',yname),res$var)
  write.csv(res,paste0('output/mcs/',yvar,'.supportstats.csv'), row.names=FALSE)
  
  mcs_all.evalues = rbind(mcs_all.evalues,res)
  hyptotest = c(hyptotest, sprintf(paste0('%sX',yname),x_names))
  
  ## save parameter estimates and conf. intervals
  write.csv(coef(supp$glm.full),paste0('output/mcs/',yvar,'.fullcoef.csv'))
  conf.int005 = confint(supp$glm.full,level=0.95)
  write.csv(conf.int005,paste0('output/mcs/',yvar,'.fullconfint005.csv'))
  conf.int001 = confint(supp$glm.full,level=0.99)
  write.csv(conf.int001,paste0('output/mcs/',yvar,'.fullconfint001.csv'))
  
  
  ## save odds ratio and conf. intervals
  write.csv(exp(coef(supp$glm.full)),paste0('output/mcs/',yvar,'.fulloddratio.csv'))
  write.csv(exp(conf.int005),paste0('output/mcs/',yvar,'.fullconfintodd005.csv'))
  write.csv(exp(conf.int001),paste0('output/mcs/',yvar,'.fullconfintodd001.csv'))
}

write.csv(mcs_all.evalues,paste0('output/mcs/','all.evalues.csv'), row.names=FALSE)


#### e-confidence intervals

for (idy in 1:length(yvars)){
  yvar = yvars[idy]; yname = names(yvars)[idy]
  cat('Analysing outcome:',yname,'\n')
  datareg = na.omit(data[c(yvar, x_vars, cvars)])
  datareg[datareg[cvars[names(cvars)=='Father']]==2,]=0
  names(datareg) = c('y', x_names, names(cvars))
  
  #interaction.terms = unlist(lapply(x_names, function(x) sprintf(paste0(x,':%s'),names(cvars))))
  my.formula.string = paste('y', "~", paste(c(x_names,names(cvars)), collapse = " + "))
  my.formula= as.formula(my.formula.string)
  
  ### e-conf. intervals
  e.conf.int005 = e.conf.int(vars=x_names, my.formula, datareg, family ='binomial', level=0.05, grid.up.width = 200, grid.low.width = 20)
  write.csv(e.conf.int005,paste0('output/mcs/',yvar,'.fullEconfint005.csv'))
  e.conf.int001 = e.conf.int(vars=x_names, my.formula, datareg, family ='binomial', level=0.01, grid.up.width = 200, grid.low.width = 20)
  write.csv(e.conf.int001,paste0('output/mcs/',yvar,'.fullEconfint001.csv'))
  
  
  ## save odds ratio conf. intervals
  e.conf.int.odds005 = e.conf.int005
  e.conf.int.odds005$down = exp(as.numeric(e.conf.int005$down))
  e.conf.int.odds005$up = exp(as.numeric(e.conf.int005$up))
  write.csv(e.conf.int.odds005,paste0('output/mcs/',yvar,'.fullEconfintodds005.csv'))
  e.conf.int.odds001 = e.conf.int001
  e.conf.int.odds001$down = exp(as.numeric(e.conf.int001$down))
  e.conf.int.odds001$up = exp(as.numeric(e.conf.int001$up))
  write.csv(e.conf.int.odds001,paste0('output/mcs/',yvar,'.fullEconfintodds001.csv'))
  #write.csv(confint(supp$glm.full,level=1-(1/0.05+1)^(-2)),paste0('output/mcs/',yvar,'.fullEconfint005.csv'))
  #write.csv(confint(supp$glm.full,level=1-(1/0.01+1)^(-2)),paste0('output/mcs/',yvar,'.fullEconfint001.csv'))
}

#### Computing eBH corrected e-values for all outcomes ####
for(meth in c('logcalib1','logcalib2','logmixtevalue','logsoftevalue')){
  mcs_evalues.hyptotest = mcs_all.evalues[mcs_all.evalues$hyp%in% hyptotest,]
  mcs_all.eBH = eBH.ksmall(exp(mcs_evalues.hyptotest[,meth]),mcs_evalues.hyptotest[,'hyp'],0.05)
  mcs_all.eBH['outcome'] = sapply(strsplit(mcs_all.eBH$hyp,'X'), function(x) x[[2]])
  mcs_all.eBH['var'] = sapply(strsplit(mcs_all.eBH$hyp,'X'), function(x) x[[1]])
  write.csv(mcs_all.eBH, paste0('output/mcs/all.eBH005',meth,'.csv'), row.names=FALSE)
  print(compute_cebh_discovery_set(exp(mcs_evalues.hyptotest[,meth]),
                                   mcs_evalues.hyptotest$hyp,0.05))
  
}
detach(mcs)
save.image('output/mcs/env.image.Rdata')
