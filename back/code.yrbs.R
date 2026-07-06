### Loading data
setwd("~/GitHub/multiverse.evalues/")
source('routines.R')
yrbs = new.env()
load('data/yrbs.Rdata', yrbs)
set.seed(35)

attach(yrbs)
names(y)
names(x)
cvars
cvarsplus

################################################################################
#           eBH-corrected universal mixture evalue - yrbs data                 #
################################################################################

#### Computing raw and eBH corrected e-values for individual outcomes ####
yrbs_ind.evalues = list(); yrbs_ind.eBH = list(); yrbs_loglik = list()
yrbs_all.evalues = data.frame()
hyptotest = c()

idy=2
yname = names(y)[idy]
cat('Analysing outcome:',yname)
idx = c(1,2)
datareg = data.frame(y[,idy], x[,idx], data[,c(cvars,cvarsplus)])
names(datareg) = c('y', names(x)[idx], c_names)  # set names
datareg = datareg[rowSums(is.na(datareg))==0, ]  # remove NAs

###### computing raw universal mixture evalue
res = unimixevalue(formula=y ~ ., data=datareg, family='binomial', vars=names(x), BF=TRUE)
res$evalues['yvar'] = yname
res$evalues['hyp'] = sprintf(paste0('%sX',yname),res$evalues$var)
hyptotest = c(hyptotest, sprintf(paste0('%sX',yname),names(x)))
write.csv(res$evalues,paste0('output/yrbs/',yname,'.evalues.csv'), row.names=FALSE)

#### Computing eBH corrected e-values for all outcomes ####
yrbs_evalues.hyptotest = res$evalues[res$evalues$hyp%in% hyptotest,] 
yrbs_all.eBH = eBH(yrbs_evalues.hyptotest$evalue,yrbs_evalues.hyptotest$hyp,0.1)
yrbs_all.eBH['outcome'] = sapply(strsplit(yrbs_all.eBH$hyp,'X'), function(x) x[[2]])
yrbs_all.eBH['var'] = sapply(strsplit(yrbs_all.eBH$hyp,'X'), function(x) x[[1]])
write.csv(yrbs_all.eBH, paste0('output/yrbs/all.eBH.csv'), row.names=FALSE)
detach(yrbs)
save.image('output/yrbs/env.image.Rdata')
