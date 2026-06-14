#### Helper functions ####

unimixevalue = function(formula, data, family, vars=NA, BF=FALSE){
  if(!require(modelSelection)){
    install.packages("modelSelection")
    library(modelSelection)
  }
  x.modmat = model.matrix(formula, data)
  nvars = ncol(x.modmat)
  
  ## List of models minus 1 variables + full model
  if(sum(is.na(vars))!=0){
    list.modelsminus1 = matrix(rep(TRUE,nvars^2),ncol=nvars)
    for(i in 1:nvars){
      list.modelsminus1[i,i] = FALSE
    }
    rownames(list.modelsminus1) = sprintf('min%s',colnames(x.modmat))
  }else{
    list.modelsminus1 = matrix(rep(TRUE,nvars*length(vars)),ncol=nvars)
    for(i in 1:length(vars)){
      list.modelsminus1[i,which(colnames(x.modmat)==vars[i])] = FALSE
    }
    rownames(list.modelsminus1) = sprintf('min%s',vars)
  }
  full.model = as.matrix(rep(TRUE,ncol(x.modmat)))
  
  ## marginal loglik full model - zellner unit information prior
  full.llik = marginalLikelihood(y=formula, data=data, family=family
                                 , priorCoef = zellnerprior())
  
  ## MLE log likelihood for small models
  mle.llik = data.frame(llik=rep(NA,nrow(list.modelsminus1)),modname=rep(NA,nrow(list.modelsminus1)))
  for(i in 1:nrow(list.modelsminus1)){
    if(family=='binomial'){
      glm.fitmin = glm.fit(y=data[,all.vars(formula)[1]], x=x.modmat[,list.modelsminus1[i,]], family=binomial(), intercept = FALSE)
    }
    if(family=='normal'){
    glm.fitmin = glm.fit(y=data[,all.vars(formula)[1]], x=x.modmat[,list.modelsminus1[i,]],family=gaussian(),intercept = FALSE)
    }
    mle.llik$llik[i] = -0.5*(glm.fitmin$aic-2*(sum(list.modelsminus1[i,])+1))
    mle.llik$modname[i] = rownames(list.modelsminus1)[i]
  }
  
  # mle.llik = bestIC(y=formula, data=data, family=family, 
  #               models = list.modelsminus1, penalty = 0, verbose=FALSE)$models
  # mle.llik$llik = -0.5 * mle.llik$ic
  # mle.llik$modname = unname(sapply(mle.llik$modelid,get.modname2,cols=colnames(x.modmat)))
  
  evalues = mle.llik
  evalues['evalue'] =  exp(full.llik-mle.llik['llik'])
  evalues['logevalue'] =  full.llik-mle.llik['llik']
  evalues['var'] = substring(rownames(list.modelsminus1), first=4)
  if(BF){
    evalues['BF'] = rep(NA,nrow(list.modelsminus1))
    ## marginal loglik for small model- zellner unit information prior
    small.llik = data.frame(modname=row.names(list.modelsminus1))
    small.llik['llik'] = apply(list.modelsminus1, MARGIN = 1, FUN = marginalLikelihood, 
                               y=data[,all.vars(formula)[1]], x=x.modmat, 
                               family=family, priorCoef = zellnerprior())
    evalues['BF'] = exp(full.llik-small.llik['llik'])
  }
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