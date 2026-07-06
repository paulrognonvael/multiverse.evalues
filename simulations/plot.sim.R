library(readr)
library(stringr)
library(tidyverse)
library(ggplot2)
library(latex2exp)

wd="~/GitHub/multiverse.evalues/simulations/"
setwd(wd)



beta = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.5, 0.75, 1)

##### Logistic regression ########

summar.log <- read_csv("~/GitHub/multiverse.evalues/simulations/logreg/summaries.all.csv", 
                       col_types = cols(sim = col_skip(), n = col_integer()))

for(i in 1:length(beta)){
  betai = colnames(summar.log)[i]
  summar.log %>% select(all_of(c(betai,'n','type'))) %>% 
    filter(type%in%c('mixt','calib1','calib3','soft')) %>% 
    ggplot() + geom_line(aes(x=n,y=.data[[betai]], linetype = type))+
    geom_point(aes(x=n,y=.data[[betai]], shape = type)) +
    theme_light(base_size = 10)+
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(-5,-5,0,-5),
          legend.position="bottom",
          legend.key.spacing.x = unit(0, 'pt'),
          legend.key.spacing.y = unit(0, 'pt'),
          legend.key.width = unit(14, 'pt'),
          legend.key.height = unit(9, 'pt'),
          legend.text=element_text(size=10),
          legend.title = element_blank())+
    guides(linetype = guide_legend(nrow = 2))+ 
    scale_x_log10() +
    scale_linetype_manual(#name='e-var.', 
                          values = c("soft" = 1, "calib3" = 2, "calib1" = 3, "mixt"=6),
                          labels = c("soft" = "soft-rank", "calib3" = "calib. c2", "calib1" = "calib. c1", "mixt"="univ. mixt")) +
    scale_shape_manual(#name='e-var.', 
                       values = c("soft" = 1, "calib3" = 2, "calib1" = 3, "mixt"=4),
                       labels = c("soft" = "soft-rank", "calib3" = "calib. c2", "calib1" = "calib. c1", "mixt"="univ. mixt"))+
    ylab('log e-value')
    #ggtitle(TeX(sprintf(r"($\beta_i=%s$)",beta[i])))
    ggsave(paste0('logreg/sim.logreg.paper.beta',beta[i],'.png'),width =60, height = 60, units='mm')
    
    summar.log %>% select(all_of(c(betai,'n','type'))) %>% 
      filter(!type%in%c('pvalues','calib2','calib4')) %>% 
      ggplot() + geom_line(aes(x=n,y=.data[[betai]], linetype = type))+
      geom_point(aes(x=n,y=.data[[betai]], shape = type)) +
      theme_light(base_size = 10)+
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(-5,-5,0,-5),
            legend.position="bottom",
            legend.key.spacing.x = unit(0, 'pt'),
            legend.key.spacing.y = unit(0, 'pt'),
            legend.key.width = unit(14, 'pt'),
            legend.key.height = unit(9, 'pt'),
            legend.text=element_text(size=10),
            legend.title = element_blank())+
      guides(linetype = guide_legend(nrow = 2))+ 
      scale_x_log10() +
      scale_linetype_manual(name='e-var. or BF', 
                            values = c("soft" = 1, "calib3" = 2, "calib1" = 3, "mixt"=6, 'bf'= 5),
                            labels = c("soft" = "soft-rank", "calib3" = "calib. c2", "calib1" = "calib. c1", "mixt"="univ. mixt", 'bf'= 'Bayes factor')) +
      scale_shape_manual(name='e-var. or BF', 
                            values = c("soft" = 1, "calib3" = 2, "calib1" = 3, "mixt"=4, 'bf'= 5),
                            labels = c("soft" = "soft-rank", "calib3" = "calib. c2", "calib1" = "calib. c1", "mixt"="univ. mixt", 'bf'= 'Bayes factor'))+
    ylab('log e-value or Bayes factor')
      # ggtitle(TeX(sprintf(r"($\beta_i=%s$)",beta[i])))
    ggsave(paste0('logreg/sim.logreg.allev.beta',beta[i],'.png'),width =60, height = 60, units='mm')
  
}

