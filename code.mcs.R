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
  res = unimixevalue(formula=my.formula, data=datareg, family='binomial', vars=x_names, BF=TRUE)
  res$evalues['yvar'] = yname
  res$evalues['hyp'] = sprintf(paste0('%sX',yname),res$evalues$var)
  write.csv(res$evalues,paste0('output/mcs/',yvar,'.evalues.csv'), row.names=FALSE)
  
  mcs_all.evalues = rbind(mcs_all.evalues,res$evalues)
  hyptotest = c(hyptotest, sprintf(paste0('%sX',yname),x_names))
}
write.csv(mcs_all.evalues,paste0('output/mcs/','all.evalues.csv'), row.names=FALSE)

#### Computing eBH corrected e-values for all outcomes ####
mcs_evalues.hyptotest = mcs_all.evalues[mcs_all.evalues$hyp%in% hyptotest,] 
mcs_all.eBH = eBH(mcs_evalues.hyptotest$evalue,mcs_evalues.hyptotest$hyp,0.1)
mcs_all.eBH['outcome'] = sapply(strsplit(mcs_all.eBH$hyp,'X'), function(x) x[[2]])
mcs_all.eBH['var'] = sapply(strsplit(mcs_all.eBH$hyp,'X'), function(x) x[[1]])
write.csv(mcs_all.eBH, paste0('output/mcs/all.eBH.csv'), row.names=FALSE)
detach(mcs)
save.image('output/mcs/env.image.Rdata')
