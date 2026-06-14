library(modelSelection)
library(mvtnorm)
set.seed(35)

sim.logistic = function(n,p=30){
  x = rmvnorm(n, mean=rep(0,p))
  beta = c(runif(2,1/2,2/3),1:5,rep(0,p-7))
  y = rbinom(n,1, 1/(1+exp(-x %*% beta )))
  datareg = data.frame(y=y)
  datareg = cbind(datareg, x)
  return(datareg)
}

n.sim =800
p.sim = 20
datareg = sim.logistic(n=n.sim,p=p.sim)

formula = y~.
x.modmat = model.matrix(y~., datareg)


# Comparing the maximized likelihood output by glm.fit (through its computation of aic) and the one output by bestIC 
### in the binomial family
glm.fit = glm.fit(y=datareg[,all.vars(formula)[1]], x=x.modmat,family=binomial(),intercept = FALSE)
-0.5*(glm.fit$aic-2*(ncol(x.modmat)))

list.model = matrix(rep(TRUE,ncol(datareg[,-1])),ncol=ncol(datareg[,-1]))
llik = bestIC(y=datareg$y, x=datareg[,-1], family='binomial', 
              models = list.model, penalty = 0)$models
-0.5 * llik$ic
#match

### in the Gaussian family
glm.fit = glm.fit(y=datareg$y, x=x.modmat, family=gaussian(),intercept = FALSE)
-0.5*(glm.fit$aic-2*(ncol(x.modmat)+1))

list.model = matrix(rep(TRUE,ncol(x.modmat)),ncol=ncol(x.modmat))
llik = bestIC(y=datareg$y, x=x.modmat, family='normal', 
              models = list.model, penalty = 0)$models
-0.5 * llik$ic


#### Comparing the maximized likelihood output by glm.fit and marginal likelhood under zellner prior 
### in the binomial family
glm.fit = glm(formula, data=datareg, family=binomial())
-0.5*(glm.fit$aic-2*(ncol(x.modmat)))
marginalLikelihood(y=formula, data=datareg, family='binomial', priorCoef = zellnerprior())

### in the gaussian family
glm.fit = glm(formula, data=datareg, family=gaussian())
-0.5*(glm.fit$aic-2*(ncol(x.modmat)+1))
marginalLikelihood(y=formula, data=datareg, family='normal', priorCoef = zellnerprior())
