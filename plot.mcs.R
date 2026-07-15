library(tidyverse)

### Loading data
setwd("~/GitHub/multiverse.evalues/")
mcs = new.env()
load('data/mcs.Rdata', mcs)
attach(mcs)
x_names =  c("TV", "Electronic_games", "Social_media", "Other_internet", "Own_computer")

for(yvar in yvars){
  coefreg = read_csv(paste0('output/mcs/',yvar,'.fullcoef.csv'))
  colnames(coefreg)[1] = 'xvar'
  
  ## Confidence interval
  coefreg.ci = read_csv(paste0('output/mcs/',yvar,'.fullconfint005.csv'))
  colnames(coefreg.ci) = c('xvar','lower','upper')
  df_ci <- coefreg.ci %>% filter( xvar %in% x_names)
  df_ci['point'] = coefreg %>% filter( xvar %in% x_names) %>% select(x)
  df_ci$xvar = factor(df_ci$xvar, 
                      levels = c("TV", "Electronic_games", "Social_media", "Other_internet", "Own_computer"),
                      labels =c('TV', 'Elect. games', 'Soc. media', 'Oth. internet', 'Own comp.'))
  ggplot(df_ci, aes(x = xvar, y = point)) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
    geom_hline(yintercept =0, linetype=2) +
    theme_light(base_size = 10)+
    xlab('Treatment') + ylab('Effect') +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust =1),
          axis.title = element_text(size=8))
  ggsave(paste0('output/mcs/plot.ci',yvar,'.png'), width =60, height = 60, units='mm')
  
  ## e-confidence interval
  coefreg.Eci = read_csv(paste0('output/mcs/',yvar,'.fullEconfint005.csv'))
  colnames(coefreg.Eci) = c('id','xvar','lower','upper')
  df_Eci <- coefreg.Eci %>% filter( xvar %in% x_names)
  df_Eci['point'] = coefreg %>% filter( xvar %in% x_names) %>% select(x)
  df_Eci$xvar = factor(df_Eci$xvar, 
                       levels = c("TV", "Electronic_games", "Social_media", "Other_internet", "Own_computer"),
                       labels =c('TV', 'Elect. games', 'Soc. media', 'Oth. internet', 'Own comp.'))
  ggplot(df_Eci, aes(x = xvar, y = point)) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
    geom_hline(yintercept =0, linetype=2) +
    theme_light(base_size = 10)+
    xlab('Treatment') + ylab('Effect') +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust =1),
          axis.title = element_text(size=8))
  ggsave(paste0('output/mcs/plot.Eci',yvar,'.png'), width =60, height = 60, units='mm')
}
