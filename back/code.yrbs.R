### Loading data
setwd("~/GitHub/multiverse.evalues/")
source('routines.R')
yrbs = new.env()
load('data/yrbs.Rdata', yrbs)
set.seed(35)

attach(yrbs)
names(yvars) 
names(cvars)

################################################################################
#                              yrbs data                                        #
################################################################################

#### Computing raw and eBH corrected e-values for individual outcomes ####
yrbs_ind.evalues = list(); yrbs_ind.eBH = list(); yrbs_loglik = list()
yrbs_all.evalues = data.frame()

idy=2
yname = names(yvars)[idy]
cat('Analysing outcome:',yname)
idx = c(1,2)
datareg = data.frame(y[,idy], x[,idx], data[,c(cvars,cvarsplus)])
names(datareg) = c('y', names(x)[idx], c_names)  # set names
datareg = datareg[rowSums(is.na(datareg))==0, ]  # remove NAs

###### with full likelihood model chosen with modelSelection and likelihood computed on the H0 sample
res = evalues.nosplit(formula=y ~ ., data=datareg, family='binomial')
res$loglik['yvar'] = yname
res$evalues['yvar'] = yname
write.csv(res$loglik,paste0('output/yrbs/',yname,'.loglik.csv'), row.names=FALSE)
write.csv(res$evalues,paste0('output/yrbs/',yname,'.evalues.csv'), row.names=FALSE)
write.csv(res$full.loglik,paste0('output/yrbs/',yname,'.fulloglik.csv'), row.names=FALSE)


###### with full likelihood computed with marginalLikelihood
# res2 = evalues2(y ~ ., data=datareg, family='binomial')
# res2$loglik['yvar'] = yname
# res2$evalues['yvar'] = yname
# write.csv(res2$loglik,paste0('output/',yname,'.loglik2.csv'), row.names=FALSE)
# write.csv(res2$evalues,paste0('output/',yname,'.evalues2.csv'), row.names=FALSE)

evalues.toBH = res$evalues[!res$evalues$var=='(Intercept)',]
evalues.toBH$hyp = sprintf(paste0('%sX',yname),evalues.toBH$var)
res.eBH = eBH(evalues.toBH$evalue,evalues.toBH$hyp,0.1)
write.csv(res.eBH,paste0('output/yrbs/',yname,'.ind.eBH.csv'), row.names=FALSE)

yrbs_loglik[[yname]] = res$loglik
yrbs_ind.evalues[[yname]] = res$evalues
yrbs_ind.eBH[[yname]] = res.eBH
yrbs_all.evalues = rbind(yrbs_all.evalues,evalues.toBH)


#### Computing eBH corrected e-values for all outcomes ####
yrbs_all.eBH = eBH(yrbs_all.evalues$evalue,yrbs_all.evalues$hyp,0.1)
yrbs_all.eBH['outcome'] = sapply(strsplit(yrbs_all.eBH$hyp,'X'), function(x) x[[2]])
yrbs_all.eBH['var'] = sapply(strsplit(yrbs_all.eBH$hyp,'X'), function(x) x[[1]])
write.csv(yrbs_all.eBH, paste0('output/yrbs/all.eBH.csv'), row.names=FALSE)
detach(yrbs)
save.image('output/yrbs/env.image.Rdata')

