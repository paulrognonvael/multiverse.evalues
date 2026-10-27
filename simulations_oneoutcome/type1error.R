library(readr)
source('~/GitHub/multiverse.evalues/routines.R')
type1error.funct = function(n,eval,reg){
  data = read_csv(paste0("~/GitHub/multiverse.evalues/simulations/",reg,"/",eval,".sim.n",n,'.csv'))
  error005 = sum(exp(data$A) > 1/0.05)/nrow(data)
  error001 = sum(exp(data$A) > 1/0.01)/nrow(data)
  return(list(e005=error005,e001=error001))
}

# FDR.funct = function(n,eval,reg){
#   data = read_csv(paste0("~/GitHub/multiverse.evalues/simulations/",reg,"/",eval,".sim.n",n,'.csv'))
#   eBH005 = eBH.ksmall(exp(data$A),1:nrow(data),0.05)
#   FDR005 = 
#   eBH001=eBH.ksmall(exp(data$A),1:nrow(data),0.01)
#   return(list(e005=error005,e001=error001,eBH005=eBH005,eBH001=eBH001))
# }


#### Logistic regression ####
type1erro.logreg.df = data.frame()
for(eval in c('logsoftevalues','logmixtevalues','logcalib1','logcalib2','logcalib3','logcalib4')){
  for(n in c(100, 250, 500, 750, 1000, 1500, 2000, 2500, 5000, 10000)){
    type1error.eval.n =type1error.funct(n,eval,'logreg')
    type1erro.logreg.df = rbind(type1erro.logreg.df,c(eval,n,type1error.eval.n$e005,type1error.eval.n$e001))
  }
}
colnames(type1erro.logreg.df) = c('n','eval','type1error0.05','type1error0.01')
write.csv(type1erro.logreg.df,'~/GitHub/multiverse.evalues/simulations/logreg/type1error.logreg.csv',row.names = FALSE)

#### Linear regression ####
type1erro.linreg.df = data.frame()
for(eval in c('logsoftevalues','logmixtevalues','logcalib1','logcalib2','logcalib3','logcalib4')){
  for(n in c(100, 250, 500, 750, 1000, 1500, 2000, 2500, 5000, 10000)){
    type1error.eval.n =type1error.funct(n,eval,'linreg')
    type1erro.linreg.df = rbind(type1erro.linreg.df,c(eval,n,type1error.eval.n$e005,type1error.eval.n$e001))
  }
}
colnames(type1erro.linreg.df) = c('n','eval','type1error0.05','type1error0.01')
write.csv(type1erro.linreg.df,'~/GitHub/multiverse.evalues/simulations/linreg/type1error.linreg.csv',row.names = FALSE)
