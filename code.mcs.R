### Loading data
setwd("~/github/multiverse.evalues/")
source('routines.R')
mcs = new.env()
load('data/mcs.Rdata', mcs)
set.seed(35)

library(stringr)
library(tidyverse)

attach(mcs)
names(yvars) 
names(cvars)
x_names


################################################################################
#           REPORT MISSING OBSERVATIONS                                        #
################################################################################

cat(nrow(data), "observations")

x_names =  c("TV", "Electronic_games", "Social_media", "Other_internet", "Own_computer")

nrow(data) - colSums(is.na(data[yvars]))  # observations reporting the outcome variables
colSums(is.na(data[x_vars])) # missings in treatment variables
colSums(is.na(data[cvars]))  # missings in control variables

sum(rowSums(is.na(data[c(x_vars,cvars)])) == 0) # observations with no missings in treatments nor controls

sum(rowSums(is.na(data[x_vars])) > 0)  # individuals missing at least one treatment
sum(rowSums(is.na(data[cvars])) > 0)  # individuals missing at least one control


outcome.reported = complete.obs = integer(length(yvars))
for (idy in 1:length(yvars)) {
  yvar = yvars[idy]; yname = names(yvars)[idy]
  datareg = na.omit(data[c(yvar, x_vars, cvars)])
  outcome.reported[idy] = nrow(data) - sum(is.na(data[yvar]))
  complete.obs[idy] = nrow(datareg)
}

nmiss = data.frame(variable = names(yvars), outcome.reported, complete.obs) |>
  mutate(perc.complete.obs = round(100 * complete.obs / outcome.reported, 1))

xtable::xtable(nmiss[,1:3])


################################################################################
#           Obtain calibrated mixture evalues on MCS data                      #
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
  res = supp$stats[,c('var','logcalib1','logcalib3','logmixtevalue','logsoftevalue','anov.pvalue')]
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
for(meth in c('logcalib1','logcalib3','logmixtevalue','logsoftevalue')){
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




# Final table with significant results (including Benjamini-Yekutieli adjusted P-values)

## Import P-values, point estimates and 95% intervals for all (outcome, treatment) combinations
supportstats = vector("list", length(yvars))
names(supportstats) = yvars
for (yvar in yvars) {
  
  pvals= read_csv(paste0('output/mcs/',yvar,'.supportstats.csv')) |>
    rename(xvar = var, pvalue = anov.pvalue) |>
    select(yvar, xvar, pvalue)
  
  coefreg = read_csv(paste0('output/mcs/',yvar,'.fullcoef.csv'))
  colnames(coefreg) = c('xvar', 'logOR')
  
  coefreg.ci = read_csv(paste0('output/mcs/',yvar,'.fullEconfintodds005.csv'))[,-1]
  colnames(coefreg.ci) = c('xvar','lower','upper')
  
  coefreg = merge(coefreg, coefreg.ci, by='xvar') |>
    filter(xvar %in% c("TV", "Electronic_games", "Social_media", "Other_internet", "Own_computer"))
  
  supportstats[[yvar]] = merge(pvals, coefreg, by='xvar')
  
}

supportstats = do.call(rbind, supportstats) |>
  mutate(pvalue.BY= p.adjust(pvalue, method='BY')) |>
  select(yvar, everything()) |>
  rename(Outcome = yvar, Treatment = xvar)

supportstats = mutate(supportstats, OR= exp(logOR), CI = paste0("(", round(lower,2), ",", round(upper, 2), ")")) |>
  select(Outcome, Treatment, OR, CI, pvalue, pvalue.BY)

filter(supportstats, pvalue.BY < 0.05)

## Obtain adjusted e-values and merge them with p-values into a single data.frame

library(eClosure) # closed e-BH procedure

evalues = read_csv(paste0('output/mcs/all.eBH005logcalib1.csv')) |>
  rename(Outcome = outcome, Treatment = var) |>
  select(Outcome, Treatment, everything()) |>
  arrange(desc(evalue))

k_bar <- closedeBH(evalues$evalue, alpha = 0.05) # Size of the largest rejection set k_bar
evalues <- mutate(evalues, closed_eBH= 1:nrow(evalues) <= k_bar)


tab = merge(evalues, supportstats, by=c('Outcome', 'Treatment')) |>
  arrange(desc(evalue)) |>
  select(Outcome, Treatment, OR, CI, evalue, pvalue.BY, pvalue, closed_eBH)

filter(tab, closed_eBH | pvalue.BY < 0.05)  # tests rejected either by closed e-BH or by BY


group_by(tab, Treatment) |>
  summarize(evalue = mean(evalue))

group_by(tab, Treatment) |>
  summarize(min_pvalue= min(pvalue), number_outcomes=n()) |>
  mutate(reject_Bonferroni = min_pvalue * number_outcomes < 0.05)




################################################################################
##  COMBINE EVIDENCE WITH THAT FROM TWENGE ET AL 2017
################################################################################

p2e.mix <- function(p) (1 - p + p * log(p)) / (p * (log(p)^2))  # p-to-e mixture calibrator

# Function to convert partial correlation r to exact p-value and calibrated e-value
r_to_p_e <- function(r, n, k = 5) {
  df <- n - 2 - k
  t_stat <- r * sqrt(df / (1 - r^2))
  p_val <- 2 * pt(abs(t_stat), df = df, lower.tail = FALSE)
  e_val <- p2e.mix(p_val)
  return(data.frame(r = r, n = n, df = df, t_stat = t_stat, p_val = p_val, e_val = e_val))
}


# Dataset. Monitoring the Future (MtF): Depressive Symptoms (Table 2)
# Controls: sex, race, SES, grade, region (k = 5 for overall; k = 4 for stratified)

evalues.mtf <- rbind(
  data.frame(Dataset = "MtF", Outcome = "Depressed (adolescent)", Treatment = "Social_media",  evalue.mtf=r_to_p_e(r = 0.03, n = 77487, k = 5)$e_val
), 
  data.frame(Dataset = "MtF", Outcome = "Depressed (adolescent)", Treatment = "TV",  evalue.mtf=r_to_p_e(r = 0.02, n = 77487, k = 5)$e_val))

#  data.frame(Dataset = "MtF", Outcome = "Depressive Symptoms", Group = "Girls", Treatment = "Social Media",      r_to_p_e(r = 0.06, n = 37830, k = 4)),
#  data.frame(Dataset = "MtF", Outcome = "Depressive Symptoms", Group = "Girls", Treatment = "TV Viewing",        r_to_p_e(r = 0.03, n = 37830, k = 4)),
#  data.frame(Dataset = "MtF", Outcome = "Depressive Symptoms", Group = "Boys",  Treatment = "Social Media",      r_to_p_e(r = 0.01, n = 36291, k = 4)),
#  data.frame(Dataset = "MtF", Outcome = "Depressive Symptoms", Group = "Boys",  Treatment = "TV Viewing",        r_to_p_e(r = 0.02, n = 36291, k = 4)) 


# Dataset. Youth Risk Behavior Surveillance System (YRBSS): Suicide-Related Risk (Table 2)
# Controls: sex, race, grade (k = 3 for overall; k = 2 for stratified)
#yrbss <- rbind(
#  data.frame(Dataset = "YRBSS", Outcome = "Suicide-Related Risk", Treatment = "Electronic Device", evalue=r_to_p_e(r = 0.13, n = 59115, k = 3)),
#  data.frame(Dataset = "YRBSS", Outcome = "Suicide-Related Risk", Treatment = "TV",  evalue=r_to_p_e(r = 0.02, n = 59115, k = 3)))

#  data.frame(Dataset = "YRBSS", Outcome = "Suicide-Related Risk", Group = "Girls", Treatment = "Electronic Device", r_to_p_e(r = 0.14, n = 29500, k = 2)),
#  data.frame(Dataset = "YRBSS", Outcome = "Suicide-Related Risk", Group = "Girls", Treatment = "TV Viewing",        r_to_p_e(r = 0.02, n = 29500, k = 2)),
#  data.frame(Dataset = "YRBSS", Outcome = "Suicide-Related Risk", Group = "Boys",  Treatment = "Electronic Device", r_to_p_e(r = 0.12, n = 29500, k = 2)),
#  data.frame(Dataset = "YRBSS", Outcome = "Suicide-Related Risk", Group = "Boys",  Treatment = "TV Viewing",        r_to_p_e(r = 0.03, n = 29500, k = 2)) 


evalues.mcs = read_csv(paste0('output/mcs/all.eBH005logcalib1.csv')) |>
  rename(Outcome = outcome, Treatment = var, evalue.mcs = evalue) |>
  select(Outcome, Treatment, evalue.mcs) |>
  arrange(desc(evalue.mcs))

evalues.all = left_join(evalues.mtf, evalues.mcs, by= join_by(Outcome, Treatment)) |>
  mutate(evalue = evalue.mtf * evalue.mcs)

evalues.all
