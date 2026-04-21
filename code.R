#install.packages('modelSelection'

#### Helper functions ####
get.modname2 = function(string,cols){
  indexes = as.numeric(strsplit(string,',')[[1]])
  if(length(indexes)==length(cols)){
    name = 'full'
  } else {
    name = paste0('min',cols[!c(1:length(cols) %in% indexes)])
  }
  return(name)
}

get.modname = function(model,cols){
  name = paste0('min',cols[!model])
  return(name)
}

get.varname = function(string){
  return(strsplit(string,'min')[[1]][2])
}

binom.loglik <- function(y,prob){
  return(y*log(prob)+(1-y)*log(1-prob))
}

binom.loglik.n <- function(y,prob){
  sum=0
  for(i in 1:length(y)){
    sum = sum + binom.loglik(y[i],prob[i])
  }
  return(sum)
}

binom.loglik.mle.model = function(model, y, x){
  glm.fit.model = glm.fit(y, x=x[,model], family=binomial(), intercept = FALSE)
  loglik = binom.loglik.n(y,glm.fit.model$fitted.values)
  return(loglik)
}




evalues = function(formula, data, family){
  if(!require(modelSelection)){
    install.packages("modelSelection")
    library(modelSelection)
  }
  x.modmat = model.matrix(formula, data)
  nvars = ncol(x.modmat)
  
  ## List of models minus 1 variables + full model
  list.models = expand.grid(replicate(nvars, c(FALSE,TRUE), simplify=FALSE))
  list.modelsminus1 = list.models[rowSums(list.models)==nvars-1,]
  list.modelsminus1 = as.matrix(list.modelsminus1)
  full.model = as.matrix(list.models[rowSums(list.models)==nvars,])
  
  ## Sample splitting
  partition = sample(c(TRUE, FALSE), nrow(data), replace=TRUE)
  
  ## Selection of full model on sample 1
  full.llik.model = modelSelection(y=formula, data=data[partition,], family=family,
                                   models = full.model)
  
  ## Evaluation of loglik full model on sample 0
  pred = predict(full.llik.model, data = data[partition,], newdata = data[!partition,])
  prob.pred = 1/(1+exp(-pred))
  full.llik = binom.loglik.n(data$y[!partition],prob.pred[,1])
  
  ## MLE likelihood on sample 0 for small models
  llik.val = apply(list.modelsminus1, MARGIN = 1, FUN = binom.loglik.mle.model,
               y=data$y[!partition], x=x.modmat[!partition,])
  llik = data.frame(llik=llik.val)
  llik$modname = unname(apply(list.modelsminus1, 1,get.modname,cols=colnames(x.modmat)))
  evalues = llik[,c('modname','llik')] 
  evalues['evalue'] =  exp(full.llik-llik['llik'])
  evalues['var'] = unname(sapply(evalues$modname,get.varname))
  evalues=evalues[,c('var','evalue')]
  return(list(evalues=evalues,loglik=llik, full.loglik= full.llik))
}



###### with full likelihood computed with marginalLikelihood
# evalues2 = function(formula, data, family){
#   if(!require(modelSelection)){
#     install.packages("modelSelection")
#     library(modelSelection)
#   }
#   x.modmat = model.matrix(formula, data)
#   nvars = ncol(x.modmat)
#   
#   ## List of models minus 1 variables + full model
#   list.models = expand.grid(replicate(nvars, c(FALSE,TRUE), simplify=FALSE))
#   list.modelsminus1 = list.models[rowSums(list.models)==nvars-1,]
#   list.modelsminus1 = as.matrix(list.modelsminus1)
#   full.model = as.matrix(list.models[rowSums(list.models)==nvars,])
#                                                     
#   
#   ## Sample splitting
#   partition = sample(c(TRUE, FALSE), nrow(data), replace=TRUE)
#   
#   ## Likelihood under the full model
#   full.llik = marginalLikelihood(y=formula, data=data[partition,], family=family)
# 
#   ## List of models minus 1 variables
#   llik = bestIC(y=formula, data=data[!partition,], family=family, 
#                 models = list.modelsminus1, penalty = 0)$models
#   llik$llik = -0.5 * llik$ic
#   llik$modname = unname(sapply(llik$modelid,get.modname,cols=colnames(x.modmat)))
#   evalues = llik[,c('modname','llik')] 
#   evalues['evalue'] =  exp(full.llik-llik['llik'])
#   evalues = evalues[!evalues['modname']=='full',]
#   evalues['var'] = unname(sapply(evalues$modname,get.varname))
#   evalues=evalues[,c('var','evalue')]
#   return(list(evalues=evalues,loglik=llik))
# }

eBH = function(evalues,hyp,level){
  evalues.df = data.frame(hyp=hyp,evalue=evalues)
  evalues.df = evalues.df[order(evalues.df$evalue,decreasing = TRUE),]
  rank = 1:nrow(evalues.df)
  evalues.df['BHcrit'] = evalues.df$evalue*rank/nrow(evalues.df) >= (1/level)
  k.star = ifelse(max(evalues.df['BHcrit']==TRUE)>0,max(rank[evalues.df['BHcrit']==TRUE]),
                  0)
  evalues.df['rejected'] = 1:nrow(evalues.df) <= k.star
  return(evalues.df)
}


#### Loading data ####
setwd("~/GitHub/multiverse.evalues")
mcs = new.env()
load('data/mcs.Rdata', mcs)

attach(mcs)
names(yvars) 
names(cvars)

#### Computing raw and eBH corrected e-values for individual outcomes ####
mcs_ind.evalues = list(); mcs_ind.eBH = list(); mcs_loglik = list()
mcs_all.evalues = data.frame()
for (idy in 1:length(yvars)){
  yvar = yvars[idy]; yname = names(yvars)[idy]
  cat('Analysing outcome:',yname)
  datareg = na.omit(data[c(yvar, x_vars, cvars)])
  names(datareg) = c('y', x_names, names(cvars))
  
  ###### with full likelihood model chosen with modelSelection and likelihood computed on the H0 sample
  res = evalues(y ~ ., data=datareg, family='binomial')
  res$loglik['yvar'] = yname
  res$evalues['yvar'] = yname
  write.csv(res$loglik,paste0('output/',yvar,'.loglik.csv'), row.names=FALSE)
  write.csv(res$evalues,paste0('output/',yvar,'.evalues.csv'), row.names=FALSE)
  write.csv(res$full.loglik,paste0('output/',yvar,'.fulloglik.csv'), row.names=FALSE)
    
  
  ###### with full likelihood computed with marginalLikelihood
  # res2 = evalues2(y ~ ., data=datareg, family='binomial')
  # res2$loglik['yvar'] = yname
  # res2$evalues['yvar'] = yname
  # write.csv(res2$loglik,paste0('output/',yvar,'.loglik2.csv'), row.names=FALSE)
  # write.csv(res2$evalues,paste0('output/',yvar,'.evalues2.csv'), row.names=FALSE)
  
  evalues.toBH = res$evalues[!res$evalues$var=='(Intercept)',]
  evalues.toBH$hyp = sprintf(paste0('%sX',yname),evalues.toBH$var)
  res.eBH = eBH(evalues.toBH$evalue,evalues.toBH$hyp,0.1)
  write.csv(res.eBH,paste0('output/',yvar,'.ind.eBH.csv'), row.names=FALSE)
  
  mcs_loglik[[yname]] = res$loglik
  mcs_ind.evalues[[yname]] = res$evalues
  mcs_ind.eBH[[yname]] = res.eBH
  mcs_all.evalues = rbind(mcs_all.evalues,evalues.toBH)
}

#### Computing eBH corrected e-values for all outcomes ####
mcs_all.eBH = eBH(mcs_all.evalues$evalue,mcs_all.evalues$hyp,0.1)
mcs_all.eBH['outcome'] = sapply(strsplit(mcs_all.eBH$hyp,'X'), function(x) x[[2]])
mcs_all.eBH['var'] = sapply(strsplit(mcs_all.eBH$hyp,'X'), function(x) x[[1]])
write.csv(mcs_all.eBH, paste0('output/all.eBH.csv'), row.names=FALSE)
detach(mcs)
save.image('output/env.image.Rdata')

