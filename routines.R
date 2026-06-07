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
  rownames(list.modelsminus1) = sprintf('min%s',rev(colnames(x.modmat)))
  full.model = as.matrix(list.models[rowSums(list.models)==nvars,])
  
  ## marginal loglik full model - zellner unit information prior
  full.llik = marginalLikelihood(y=formula, data=data, family=family
                                 , priorCoef = zellnerprior()
                                 )
  
  ## MLE log likelihood for small models
  mle.llik = bestIC(y=formula, data=data, family=family, 
                models = list.modelsminus1, penalty = 0, verbose=FALSE)$models
  mle.llik$llik = -0.5 * mle.llik$ic
  mle.llik$modname = unname(sapply(mle.llik$modelid,get.modname2,cols=colnames(x.modmat)))
  
  ## marginal loglik for small model- zellner unit information prior
  small.llik = data.frame(modname=row.names(list.modelsminus1))
  small.llik['llik'] = apply(list.modelsminus1, MARGIN = 1, FUN = marginalLikelihood, 
                     y=data[,all.vars(formula)[1]], x=x.modmat, 
                     family=family
                     , priorCoef = zellnerprior()
                     )
  
  evalues = mle.llik[,c('modname','llik')] 
  evalues['evalue'] =  exp(full.llik-mle.llik['llik'])
  evalues['var'] = unname(sapply(evalues$modname,get.varname))
  evalues = evalues[order(evalues$modname),]
  evalues['BF'] = exp(full.llik-small.llik[order(small.llik$modname),'llik'])
  evalues=evalues[,c('var','evalue','BF')]
  evalues=evalues[order(evalues$var),]
  return(list(evalues=evalues,loglik=mle.llik, full.loglik= full.llik))
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