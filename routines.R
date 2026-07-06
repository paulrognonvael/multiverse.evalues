#### Helper functions ####

drop_one_formulas <- function(formula,vars) {
  tt <- terms(formula)
  response <- as.character(attr(tt, "variables"))[2]
  terms <- attr(tt, "term.labels")
  minterms = terms[terms%in%vars]
  
  lapply(minterms, function(minterm) {
    rhs <- terms[-which(terms == minterm)]
    
    if (length(rhs) == 0) {
      as.formula(paste(response, "~ 1"))
    } else {
      as.formula(paste(response, "~", paste(rhs, collapse = " + ")))
    }
  })
}

permuted.LRT = function(formula,x1,family,data,e,x1hat,y.rest.form){
  eperm= sample(e, size=length(e), replace=FALSE)
  x1tilde= x1hat + eperm
  data.perm =  data
  data.perm[x1] =  x1tilde
  llik= logLik(glm(formula,family,data.perm))#-logLik(glm(y.rest.form,family,data.perm))
  return(llik)
}

lmResPerm.onevar = function(x1, formula, data, family, B=5000){
  if(!require(parallel)){
    install.packages("parallel")
    library(parallel)
  }
  tt <- terms(formula)
  terms <- attr(tt, "term.labels")
  response <- as.character(attr(tt, "variables"))[2]
  rest = terms[-which(terms == x1)]
  x1.rest.form = as.formula(paste(x1, "~", paste(rest, collapse = " + ")))
  y.rest.form = as.formula(paste(response, "~", paste(rest, collapse = " + ")))
  
  fit= glm(x1.rest.form, 'gaussian',data)
  x1hat= predict(fit)
  e= residuals(fit)
  bperm= double(B)
  mc= detectCores()-1
  cl <- makeCluster(mc)
  clusterExport(cl, c('formula','x1','family','data','e','x1hat','permuted.LRT','y.rest.form'),
                envir = environment())
  bperm = parLapply(cl, 1:B,fun=function(i) permuted.LRT(formula,x1,family,data,e,x1hat,y.rest.form))
  stopCluster(cl)
  # for (b in 1:B) {
  #   eperm= sample(e, size=length(e), replace=FALSE)
  #   x1tilde= x1hat + eperm
  #   data.perm =  data
  #   data.perm[x1] =  x1tilde
  #   bperm[b]= exp(logLik(glm(formula,family,data.perm))-logLik(glm(y.rest.form,family,data.perm)))
  # }
  eval = sum(exp(unlist(bperm)-logLik(glm(formula,family,data))))^(-1)#-logLik(glm(y.rest.form,family,data)))
  logeval = log(eval)
  return(c(eval,logeval))  
}

logcalib1 = function(p){
  return(log((1-p+p*log(p))/(p*log(p)^2)))
}

logcalib2 = function(p){
  return(log(2*(1-p)))
}

logcalib3 = function(p){
  return(log(p^(-1/2)-1))
}

logcalib4 = function(p){
  return(log(-log(p)))
}


hypsupp = function(formula, data, family, vars=NA, softrank=TRUE, mixtevalue=FALSE, BF=FALSE, 
                   p.to.e=FALSE){
  if(!require(modelSelection)){
    install.packages("modelSelection")
    library(modelSelection)
  }
  
  family.glm = family
  if(family.glm=='normal'){family.glm='gaussian'}
  
  ## List of small models, minus 1 variable
  if(sum(is.na(vars))!=0){vars=attr(terms(formula), "term.labels")}  
  list.modelsminus1 = drop_one_formulas(formula,vars)
  names(list.modelsminus1) = sprintf('min%s',vars)
  
  hypsupp = data.frame(modname = names(list.modelsminus1), var = vars)
  
  if(mixtevalue | BF){
    ## marginal loglik full model - zellner unit information prior
    full.marg.llik = marginalLikelihood(y=formula, data=data, family=family, 
                                        priorCoef = zellnerprior())
  }
  
  if(softrank){
    hypsupp['softevalue']=rep(NA,length(list.modelsminus1))
    hypsupp['logsoftevalue']=rep(NA,length(list.modelsminus1))
    for(i in 1:length(vars)){
      hypsupp[i,c('softevalue','logsoftevalue')]=lmResPerm.onevar(vars[i], formula=formula,
                                                                  data=data, 
                                                                  family=family.glm, 
                                                                  B=5000)
    }
  }

  if(mixtevalue){
    ## MLE log likelihood for small models
    hypsupp['mle.llik']=rep(NA,length(list.modelsminus1))
    for(i in 1:length(list.modelsminus1)){
      glm.fitmin = glm(formula = list.modelsminus1[[i]], data=data, family=family.glm)
      hypsupp$mle.llik[i] = as.numeric(logLik(glm.fitmin))
    }
    hypsupp['mixtevalue'] =  exp(full.marg.llik-hypsupp['mle.llik'])
    hypsupp['logmixtevalue'] =  full.marg.llik-hypsupp['mle.llik']
  }

  if(BF){
    hypsupp['marg.llik'] = rep(NA,length(list.modelsminus1))
    
    ## marginal loglik for small model- zellner unit information prior
    for(i in 1:length(list.modelsminus1)){
      hypsupp$marg.llik[i] = marginalLikelihood(y=list.modelsminus1[[i]], 
                                    data = data, family=family, priorCoef = zellnerprior())
    }
    hypsupp['BF'] = exp(full.marg.llik-hypsupp['marg.llik'])
    hypsupp['logBF'] = full.marg.llik-hypsupp['marg.llik']
  }
  
  if(p.to.e){
    hypsupp['anov.pvalue'] = rep(NA,length(list.modelsminus1))
    hypsupp['logcalib1'] = rep(NA,length(list.modelsminus1))
    hypsupp['logcalib2'] = rep(NA,length(list.modelsminus1))
    hypsupp['logcalib3'] = rep(NA,length(list.modelsminus1))
    hypsupp['logcalib4'] = rep(NA,length(list.modelsminus1))
    ## MLE fit for full model
    glm.fitfull = glm(formula=formula, data=data, family=family.glm)
    for(i in 1:length(list.modelsminus1)){
      glm.fitmin = glm(formula = list.modelsminus1[[i]], data=data, family=family.glm)
      if(family.glm=='gaussian'){
        hypsupp$anov.pvalue[i]=anova(glm.fitfull,glm.fitmin)$`Pr(>F)`[2]        
      }
      if(family.glm=='binomial'){
        hypsupp$anov.pvalue[i]=anova(glm.fitfull,glm.fitmin)$`Pr(>Chi)`[2]        
      }
      hypsupp$logcalib1[i] = logcalib1(hypsupp$anov.pvalue[i])
      hypsupp$logcalib2[i] = logcalib2(hypsupp$anov.pvalue[i])
      hypsupp$logcalib3[i] = logcalib3(hypsupp$anov.pvalue[i])
      hypsupp$logcalib4[i] = logcalib4(hypsupp$anov.pvalue[i])
    }
  }
  
  res = hypsupp[,-which(colnames(hypsupp)%in%c('mle.llik','marg.llik','modname'))] 

  return(list(stats = res, all.res = hypsupp))
}

eBH = function(evalues,hyp,level){
  evalues.df = data.frame(hyp=hyp,evalue=evalues)
  evalues.df = evalues.df[order(evalues.df[,'evalue'],decreasing = TRUE),]
  rank = 1:nrow(evalues.df)
  evalues.df['BHcrit'] = evalues.df[,'evalue']*rank/nrow(evalues.df) >= (1/level)
  k.star = ifelse(max(evalues.df['BHcrit']==TRUE)>0,max(rank[evalues.df['BHcrit']==TRUE]),
                  0)
  evalues.df['rejected'] = 1:nrow(evalues.df) <= k.star
  return(evalues.df)
}
