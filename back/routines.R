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
    list.modelsminus1 = new_formula <- update(old_formula, . ~ . - x2)
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
  full.marg.llik = marginalLikelihood(y=formula, data=data, family=family
                                 , priorCoef = zellnerprior())
  
  ## MLE log likelihood for small models
  small.mle.llik = data.frame(llik=rep(NA,nrow(list.modelsminus1)),modname=rep(NA,nrow(list.modelsminus1)))
  for(i in 1:nrow(list.modelsminus1)){
    if(family=='binomial'){
      glm.fitmin = glm.fit(y=data[,all.vars(formula)[1]], x=x.modmat[,list.modelsminus1[i,]], family=binomial(), intercept = FALSE)
      small.mle.llik$llik[i] = -0.5*(glm.fitmin$aic-2*(sum(list.modelsminus1[i,])))
    }
    if(family=='normal'){
      glm.fitmin = glm.fit(y=data[,all.vars(formula)[1]], x=x.modmat[,list.modelsminus1[i,]],family=gaussian(),intercept = FALSE)
      small.mle.llik$llik[i] = -0.5*(glm.fitmin$aic-2*(sum(list.modelsminus1[i,])+1))
    }
    small.mle.llik$modname[i] = rownames(list.modelsminus1)[i]
  }
  
  # small.mle.llik = bestIC(y=formula, data=data, family=family, 
  #               models = list.modelsminus1, penalty = 0, verbose=FALSE)$models
  # small.mle.llik$llik = -0.5 * small.mle.llik$ic
  # small.mle.llik$modname = unname(sapply(small.mle.llik$modelid,get.modname2,cols=colnames(x.modmat)))
  
  evalues = small.mle.llik
  evalues['evalue'] =  exp(full.marg.llik-small.mle.llik['llik'])
  evalues['logevalue'] =  full.marg.llik-small.mle.llik['llik']
  evalues['var'] = substring(rownames(list.modelsminus1), first=4)
  if(BF){
    evalues['BF'] = rep(NA,nrow(list.modelsminus1))
    ## marginal loglik for small model- zellner unit information prior
    small.marg.llik = data.frame(modname=row.names(list.modelsminus1))
    small.marg.llik['llik'] = apply(list.modelsminus1, MARGIN = 1, FUN = marginalLikelihood, 
                               y=data[,all.vars(formula)[1]], x=x.modmat, 
                               family=family, priorCoef = zellnerprior())
    evalues['BF'] = exp(full.marg.llik-small.marg.llik['llik'])
  }
  
  # if(pvalue){
  #   ## MLE fit for full model
  #   if(family=='binomial'){
  #     glm.fitfull = glm.fit(y=data[,all.vars(formula)[1]], x=x.modmat, family=binomial(), intercept = FALSE)
  #   }
  #   if(family=='normal'){
  #     glm.fitfull = glm.fit(y=data[,all.vars(formula)[1]], x=x.modmat,family=gaussian(),intercept = FALSE)
  #   }
  #   
  #   anova(glm.fitfull,glm.fitmin) ?glm
  # }
  
  return(list(evalues=evalues,loglik=small.mle.llik, full.loglik= full.marg.llik))
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