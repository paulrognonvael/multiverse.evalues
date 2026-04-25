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

binom.loglik.n <- function(y,prob){
  sum=0
  for(i in 1:length(y)){
    sum = sum + y[i]*log(prob[i])+(1-y[i])*log(1-prob[i])
  }
  return(sum)
}

gauss.loglik.n <- function(y, mean, var){
  sum=0
  for(i in 1:length(y)){
    sum = sum - log(2*pi*var)/2 - (y[i]-mean[i])^2/(2*var) ##### TO EDIT
  }
  return(sum)
}

binom.loglik.mle.model = function(model, y, x){
  glm.fit.model = glm.fit(y, x=x[,model], family=binomial(), intercept = FALSE)
  loglik = binom.loglik.n(y,glm.fit.model$fitted.values)
  return(loglik)
}

gauss.loglik.mle.model = function(model, y, x){
  lm.fit.model = lm.fit(y, x=x[,model])
  loglik = gauss.loglik.n(y,lm.fit.model$fitted.values,var(lm.fit.model$residuals))
  return(loglik)
}


evalues.split = function(formula, data, family){
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
  if(family=='binomial'){
    prob.pred = 1/(1+exp(-pred))
    full.llik = binom.loglik.n(data$y[!partition],pred[,1])
  }
  if(family=='normal'){
    full.llik = gauss.loglik.n(data$y[!partition],pred[,1] #XXXXX
    )
  }
  
  ## MLE likelihood on sample 0 for small models
  if(family=='binomial'){
    llik.val = apply(list.modelsminus1, MARGIN = 1, FUN = binom.loglik.mle.model,
                     y=data$y[!partition], x=x.modmat[!partition,])
  }
  
  if(family=='normal'){
    llik.val = apply(list.modelsminus1, MARGIN = 1, FUN = gauss.loglik.mle.model,
                     y=data$y[!partition], x=x.modmat[!partition,])
  }
  
  llik = data.frame(llik=llik.val)
  llik$modname = unname(apply(list.modelsminus1, 1,get.modname,cols=colnames(x.modmat)))
  evalues = llik[,c('modname','llik')] 
  evalues['evalue'] =  exp(full.llik-llik['llik'])
  evalues['var'] = unname(sapply(evalues$modname,get.varname))
  evalues=evalues[,c('var','evalue')]
  evalues=evalues[order(evalues$var),]
  return(list(evalues=evalues,loglik=llik, full.loglik= full.llik))
}

evalues.nosplit = function(formula, data, family){
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
  
  ## loglik full model
  full.llik = marginalLikelihood(y=formula, data=data, family=family)
  
  ## MLE log likelihood for small models
  if(family=='binomial'){
    llik.val = apply(list.modelsminus1, MARGIN = 1, FUN = binom.loglik.mle.model,
                     y=data$y, x=x.modmat)
  }
  
  if(family=='normal'){
    llik.val = apply(list.modelsminus1, MARGIN = 1, FUN = gauss.loglik.mle.model,
                     y=data$y, x=x.modmat)
  }
  llik = data.frame(llik=llik.val)
  llik$modname = unname(apply(list.modelsminus1, 1,get.modname,cols=colnames(x.modmat)))
  evalues = llik[,c('modname','llik')] 
  evalues['evalue'] =  exp(full.llik-llik['llik'])
  evalues['var'] = unname(sapply(evalues$modname,get.varname))
  evalues=evalues[,c('var','evalue')]
  evalues=evalues[order(evalues$var),]
  return(list(evalues=evalues,loglik=llik, full.loglik= full.llik))
}


evalues.nosplit2 = function(formula, data, family){
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
  
  ## loglik full model
  full.llik = marginalLikelihood(y=formula, data=data, family=family)
  
  ## MLE log likelihood for small models
  llik = bestIC(y=formula, data=data, family=family, 
                models = list.modelsminus1, penalty = 0)$models
  llik$llik = -0.5 * llik$ic
  llik$modname = unname(sapply(llik$modelid,get.modname2,cols=colnames(x.modmat)))
  evalues = llik[,c('modname','llik')] 
  evalues['evalue'] =  exp(full.llik-llik['llik'])
  evalues['var'] = unname(sapply(evalues$modname,get.varname))
  evalues=evalues[,c('var','evalue')]
  evalues=evalues[order(evalues$var),]
  return(list(evalues=evalues,loglik=llik, full.loglik= full.llik))
}

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