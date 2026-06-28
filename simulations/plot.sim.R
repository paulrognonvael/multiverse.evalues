library(readr)
library(stringr)
library(tidyr)
library(ggplot2)
library(latex2exp)

wd="~/GitHub/multiverse.evalues/simulations/"
setwd(wd)



beta = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.5, 0.75, 1)

##### Logistic regression ########

summar.log <- read_csv("~/GitHub/multiverse.evalues/simulations/logreg/summaries.all.csv", 
                       col_types = cols(sim = col_skip(), n = col_integer()))
# summar.log = summar.log[ , c('(Intercept)','n',colnames(summar.log)[-c(1,22)])]
# summar.log[,'mean all_0'] = rowMeans(summar.log[,colbetaval%in%c('0','(Intercept)')])
# summar.log.res = summar.log[,c('n',LETTERS[1:7],'mean all_0')] 
#colnames(summar.log)[1:10] = beta

for(i in 1:length(beta)){
  betai = colnames(summar.log)[i]
  summar.log %>% select(all_of(c(betai,'n','type'))) %>% 
    filter(!type=='pvalues') %>% 
    #pivot_longer(cols=-n,names_to='beta*=', values_to = 'mean_evalue') %>%  
    ggplot() + geom_line(aes(x=n,y=.data[[betai]], colour = type))+
    geom_point(aes(x=n,y=.data[[betai]], colour = type)) +
    #coord_cartesian(ylim=c(-50,300))+
    #scale_y_log10() +
    scale_color_discrete(name='Variable') +
    ylab('log e-value or bf') +
    ggtitle(TeX(sprintf(r"($\beta_i=%s$)",beta[i])))
    ggsave(paste0('sim.logreg.withmixt.beta',beta[i],'.png'))
    
    summar.log %>% select(all_of(c(betai,'n','type'))) %>% 
      filter(!type%in%c('pvalues','mixt')) %>% 
      #pivot_longer(cols=-n,names_to='beta*=', values_to = 'mean_evalue') %>%  
      ggplot() + geom_line(aes(x=n,y=.data[[betai]], colour = type))+
      geom_point(aes(x=n,y=.data[[betai]], colour = type)) +
      #coord_cartesian(ylim=c(-50,300))+
      #scale_y_log10() +
      scale_color_discrete(name='Variable') +
      ylab('log e-value or bf') +
      ggtitle(TeX(sprintf(r"($\beta_i=%s$)",beta[i])))
    ggsave(paste0('sim.logreg.withoutmixt.beta',beta[i],'.png'))
  
}

