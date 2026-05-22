library(readr)
library(stringr)
library(tidyr)
library(ggplot2)

setwd("~/GitHub/multiverse.evalues")

summar.lin <- read_csv("simulations/linreg/summaries.sim.csv", 
                       col_types = cols(sim = col_skip(), n = col_integer()))
#colnames(summar.lin)[-c(1,22)] = as.numeric(sapply(colnames(summar.lin)[-c(1,22)], str_remove_all,'X'))
summar.lin = summar.lin[ , c('(Intercept)','n',order(colnames(summar.lin)[-c(1,22)]))]

summar.log <- read_csv("simulations/logreg/summaries.sim.csv", 
                       col_types = cols(sim = col_skip(), n = col_integer()))
colnames(summar.log)[-c(1,22)] = as.numeric(sapply(colnames(summar.log)[-c(1,22)], str_remove_all,'X'))
summar.log = summar.log[ , c('(Intercept)','n',order(colnames(summar.log)[-c(1,22)]))]

beta = c(1/3,1/2,2/3,1.2,1.5,1.7,2,rep(0,20-7))
colbetaval = c('(Intercept)','n',beta)
summar.lin[,'0_(mean all)'] = rowMeans(summar.lin[,colbetaval%in%c('0','(Intercept)')])


colbetaval = c('(Intercept)','n',beta)
summar.log[,'0_(mean all)'] = rowMeans(summar.log[,colbetaval%in%c('0','(Intercept)')])


summar.lin.res = summar.lin[,c('n',1:7,'0_(mean all)')] 
summar.log.res = summar.log[,c('n',1:7,'0_(mean all)')] 
colnames(summar.lin.res)[2:8] = round(c(1/3,1/2,2/3,1.2,1.5,1.7,2),3)
colnames(summar.log.res)[2:8] = round(c(1/3,1/2,2/3,1.2,1.5,1.7,2),3)

summar.lin.res[,-1] = log10(summar.lin.res[,-1])
summar.log.res[,-1] = log10(summar.log.res[,-1])


summar.lin.res %>% pivot_longer(cols=-n,names_to='beta*=', values_to = 'log10mean_evalue') %>% 
  ggplot() + geom_line(aes(x=n,y=log10mean_evalue, colour = `beta*=`)) +
  geom_point(aes(x=n,y=log10mean_evalue, colour = `beta*=`)) +
  ggtitle('Linear regression')
ggsave('sim.linreg.png')

summar.log.res %>% pivot_longer(cols=-n,names_to='beta*=', values_to = 'log10mean_evalue') %>% 
  ggplot() + geom_line(aes(x=n,y=log10mean_evalue, colour = `beta*=`))+
  geom_point(aes(x=n,y=log10mean_evalue, colour = `beta*=`)) +
  ggtitle('Logistic regression')
ggsave('sim.logreg.png')
