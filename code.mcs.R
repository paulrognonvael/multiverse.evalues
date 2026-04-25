### Loading data
setwd("~/GitHub/multiverse.evalues/")
source('routines.R')
mcs = new.env()
load('data/mcs.Rdata', mcs)
set.seed(35)

attach(mcs)
names(yvars) 
names(cvars)


################################################################################
#                       NO split evalues- mcs data                             #
################################################################################
#### Loading data ####
#### Computing raw and eBH corrected e-values for individual outcomes ####
mcs_ind.evalues = list(); mcs_ind.eBH = list(); mcs_loglik = list()
mcs_all.evalues = data.frame()
for (idy in 1:length(yvars)){
  yvar = yvars[idy]; yname = names(yvars)[idy]
  cat('Analysing outcome:',yname)
  datareg = na.omit(data[c(yvar, x_vars, cvars)])
  names(datareg) = c('y', x_names, names(cvars))
  
  ###### with full likelihood model chosen with modelSelection and likelihood computed on the H0 sample
  res = evalues.nosplit(formula=y ~ ., data=datareg, family='binomial')
  res$loglik['yvar'] = yname
  res$evalues['yvar'] = yname
  write.csv(res$loglik,paste0('output/mcs/nosplit/',yvar,'.loglik.csv'), row.names=FALSE)
  write.csv(res$evalues,paste0('output/mcs/nosplit/',yvar,'.evalues.csv'), row.names=FALSE)
  write.csv(res$full.loglik,paste0('output/mcs/nosplit/',yvar,'.fulloglik.csv'), row.names=FALSE)
  
  
  ###### with full likelihood computed with marginalLikelihood
  # res2 = evalues2(y ~ ., data=datareg, family='binomial')
  # res2$loglik['yvar'] = yname
  # res2$evalues['yvar'] = yname
  # write.csv(res2$loglik,paste0('output/',yvar,'.loglik2.csv'), row.names=FALSE)
  # write.csv(res2$evalues,paste0('output/',yvar,'.evalues2.csv'), row.names=FALSE)
  
  evalues.toBH = res$evalues[!res$evalues$var=='(Intercept)',]
  evalues.toBH$hyp = sprintf(paste0('%sX',yname),evalues.toBH$var)
  res.eBH = eBH(evalues.toBH$evalue,evalues.toBH$hyp,0.1)
  write.csv(res.eBH,paste0('output/mcs/nosplit/',yvar,'.ind.eBH.csv'), row.names=FALSE)
  
  mcs_loglik[[yname]] = res$loglik
  mcs_ind.evalues[[yname]] = res$evalues
  mcs_ind.eBH[[yname]] = res.eBH
  mcs_all.evalues = rbind(mcs_all.evalues,evalues.toBH)
}

#### Computing eBH corrected e-values for all outcomes ####
mcs_all.eBH = eBH(mcs_all.evalues$evalue,mcs_all.evalues$hyp,0.1)
mcs_all.eBH['outcome'] = sapply(strsplit(mcs_all.eBH$hyp,'X'), function(x) x[[2]])
mcs_all.eBH['var'] = sapply(strsplit(mcs_all.eBH$hyp,'X'), function(x) x[[1]])
write.csv(mcs_all.eBH, paste0('output/mcs/nosplit/all.eBH.csv'), row.names=FALSE)
detach(mcs)
save.image('output/mcs/nosplit/env.image.Rdata')


################################################################################
#                       split evalues - mcs data                               #
################################################################################
#### Loading data ####

mcs = new.env()
load('data/mcs.Rdata', mcs)
set.seed(35)

attach(mcs)
names(yvars) 
names(cvars)

#### Computing raw and eBH corrected e-values for individual outcomes ####
mcs_ind.evalues = list(); mcs_ind.eBH = list(); mcs_loglik = list()
mcs_all.evalues = data.frame()
for (idy in 1:length(yvars)){
  yvar = yvars[idy]; yname = names(yvars)[idy]
  cat('Analysing outcome:',yname)
  datareg = na.omit(data[c(yvar, x_vars, cvars)])
  names(datareg) = c('y', x_names, names(cvars))
  
  ###### with full likelihood model chosen with modelSelection and likelihood computed on the H0 sample
  res = evalues.split(formula=y ~ ., data=datareg, family='binomial')
  res$loglik['yvar'] = yname
  res$evalues['yvar'] = yname
  write.csv(res$loglik,paste0('output/mcs/split/',yvar,'.loglik.csv'), row.names=FALSE)
  write.csv(res$evalues,paste0('output/mcs/split/',yvar,'.evalues.csv'), row.names=FALSE)
  write.csv(res$full.loglik,paste0('output/mcs/split/',yvar,'.fulloglik.csv'), row.names=FALSE)
  
  
  ###### with full likelihood computed with marginalLikelihood
  # res2 = evalues2(y ~ ., data=datareg, family='binomial')
  # res2$loglik['yvar'] = yname
  # res2$evalues['yvar'] = yname
  # write.csv(res2$loglik,paste0('output/',yvar,'.loglik2.csv'), row.names=FALSE)
  # write.csv(res2$evalues,paste0('output/',yvar,'.evalues2.csv'), row.names=FALSE)
  
  evalues.toBH = res$evalues[!res$evalues$var=='(Intercept)',]
  evalues.toBH$hyp = sprintf(paste0('%sX',yname),evalues.toBH$var)
  res.eBH = eBH(evalues.toBH$evalue,evalues.toBH$hyp,0.1)
  write.csv(res.eBH,paste0('output/mcs/split/',yvar,'.ind.eBH.csv'), row.names=FALSE)
  
  mcs_loglik[[yname]] = res$loglik
  mcs_ind.evalues[[yname]] = res$evalues
  mcs_ind.eBH[[yname]] = res.eBH
  mcs_all.evalues = rbind(mcs_all.evalues,evalues.toBH)
}

#### Computing eBH corrected e-values for all outcomes ####
mcs_all.eBH = eBH(mcs_all.evalues$evalue,mcs_all.evalues$hyp,0.1)
mcs_all.eBH['outcome'] = sapply(strsplit(mcs_all.eBH$hyp,'X'), function(x) x[[2]])
mcs_all.eBH['var'] = sapply(strsplit(mcs_all.eBH$hyp,'X'), function(x) x[[1]])
write.csv(mcs_all.eBH, paste0('output/mcs/split/all.eBH.csv'), row.names=FALSE)
detach(mcs)
save.image('output/mcs/split/env.image.Rdata')


