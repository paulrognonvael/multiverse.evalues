library(readr)
library(stringr)
library(tidyr)
library(ggplot2)

setwd("~/GitHub/multiverse.evalues/simulations/")

beta = c(0.05,0.1,0.2,0.4,0.6,0.8,1,rep(0,20-7))
colbetaval = c('(Intercept)','n',beta)

summar.lin <- read_csv("linreg/summaries.sim.csv", 
                       col_types = cols(sim = col_skip(), n = col_integer()))
summar.lin = summar.lin[ , c('(Intercept)','n',colnames(summar.lin)[-c(1,22)])]
summar.lin[,'mean all_0'] = rowMeans(summar.lin[,colbetaval%in%c('0','(Intercept)')])
summar.lin.res = summar.lin[,c('n',LETTERS[1:7],'mean all_0')] 
colnames(summar.lin.res)[2:8] = round(beta[1:7],3)

summar.lin.res %>% pivot_longer(cols=-n,names_to='beta*=', values_to = 'mean_evalue') %>% 
  ggplot() + geom_line(aes(x=n,y=mean_evalue, colour = `beta*=`)) +
  geom_point(aes(x=n,y=mean_evalue, colour = `beta*=`)) +
  #coord_cartesian(ylim=c(-50,300))+
  scale_y_log10() +
  ggtitle('Linear regression - proposed evalue')
ggsave('sim.linreg.propeval.png')


summar.lin.bf <- read_csv("linreg/summaries.sim.bf.csv", 
                       col_types = cols(sim = col_skip(), n = col_integer()))
summar.lin.bf = summar.lin.bf[ , c('(Intercept)','n',colnames(summar.lin.bf)[-c(1,22)])]
summar.lin.bf[,'mean all_0'] = rowMeans(summar.lin.bf[,colbetaval%in%c('0','(Intercept)')])
summar.lin.bf.res = summar.lin.bf[,c('n',LETTERS[1:7],'mean all_0')] 
colnames(summar.lin.bf.res)[2:8] = round(beta[1:7],3)

summar.lin.bf.res %>% pivot_longer(cols=-n,names_to='beta*=', values_to = 'mean_BF') %>% 
  ggplot() + geom_line(aes(x=n,y=mean_BF, colour = `beta*=`)) +
  geom_point(aes(x=n,y=mean_BF, colour = `beta*=`)) +
  #coord_cartesian(ylim=c(-50,300))+
  scale_y_log10() +
  ggtitle('Linear regression - Bayes factor')
ggsave('sim.linreg.bf.png')


##### Logistic regression ########

summar.log <- read_csv("logreg/summaries.sim.csv", 
                       col_types = cols(sim = col_skip(), n = col_integer()))
summar.log = summar.log[ , c('(Intercept)','n',colnames(summar.log)[-c(1,22)])]
summar.log[,'mean all_0'] = rowMeans(summar.log[,colbetaval%in%c('0','(Intercept)')])
summar.log.res = summar.log[,c('n',LETTERS[1:7],'mean all_0')] 
colnames(summar.log.res)[2:8] = round(beta[1:7],3)

summar.log.res %>% pivot_longer(cols=-n,names_to='beta*=', values_to = 'mean_evalue') %>% 
  ggplot() + geom_line(aes(x=n,y=mean_evalue, colour = `beta*=`))+
  geom_point(aes(x=n,y=mean_evalue, colour = `beta*=`)) +
  #coord_cartesian(ylim=c(-50,300))+
  scale_y_log10() +
  ggtitle('Logistic regression - proposed evalue')
ggsave('sim.logreg.propeval.png')


summar.log.bf <- read_csv("logreg/summaries.sim.bf.csv", 
                          col_types = cols(sim = col_skip(), n = col_integer()))
summar.lin.bf = summar.log.bf[ , c('(Intercept)','n',colnames(summar.log.bf)[-c(1,22)])]
summar.log.bf[,'mean all_0'] = rowMeans(summar.log.bf[,colbetaval%in%c('0','(Intercept)')])
summar.log.bf.res = summar.log.bf[,c('n',LETTERS[1:7],'mean all_0')] 
colnames(summar.log.bf.res)[2:8] = round(beta[1:7],3)
#summar.log.bf.res[,-1] = log(summar.log.bf.res[,-1])

summar.log.bf.res %>% pivot_longer(cols=-n,names_to='beta*=', values_to = 'mean_BF') %>% 
  ggplot() + geom_line(aes(x=n,y=mean_BF, colour = `beta*=`))+
  geom_point(aes(x=n,y=mean_BF, colour = `beta*=`)) +
  scale_y_log10() +
  #coord_cartesian(ylim=c(-50,300))+
  ggtitle('Logistic regression - Bayes factor')
ggsave('sim.logreg.bf.png')



