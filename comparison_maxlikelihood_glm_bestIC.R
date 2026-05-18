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


### Comparing the maximized likelihood output but glm.fit (through its computation of aic)
### and the one output by bestIC in the binomial family
glm.fit = glm.fit(y=datareg$y, x=datareg[,-1],family=binomial(),intercept = TRUE)
-0.5*(glm.fit$aic-2*(p.sim))

list.model = matrix(rep(TRUE,ncol(datareg[,-1])),ncol=ncol(datareg[,-1]))
llik = bestIC(y=datareg$y, x=datareg[,-1], family='binomial', 
              models = list.model, penalty = 0)$models
-0.5 * llik$ic
#match


### They match in the Gaussian family too
glm.fit = glm.fit(y=datareg$y, x=datareg[,-1], family=gaussian(),intercept = TRUE)
-0.5*(glm.fit$aic-2*(p.sim+1))

list.model = matrix(rep(TRUE,ncol(datareg[,-1])),ncol=ncol(datareg[,-1]))
llik = bestIC(y=datareg$y, x=datareg[,-1], family='normal', 
              models = list.model, penalty = 0)$models
-0.5 * llik$ic
