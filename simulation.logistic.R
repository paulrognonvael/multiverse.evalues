library(mvtnorm)
setwd("~/GitHub/multiverse.evalues/")
source('routines.R')
set.seed(35)

sim.logistic.function(n,p=30){
  x = rmvnorm(n, mean=rep(0,p))
  beta = c(runif(5,1/2,2/3),runif(5,5,5),rep(0,10))
  y = rbinom(n, ,1, 1/(1+exp(-x %*% beta )))
  datareg = data.frame(y=y)
  datareg = cbind(datareg, x)
}




#### Logistic regression 





res.split = evalues.split(formula=y~., data=datareg, family='binomial')
res.split$evalues

res.nosplitmanual = evalues.nosplit(formula=y~., data=datareg, family='binomial')
res.nosplitmanual$evalues

res.nosplitbestIC = evalues.nosplit2(formula=y~., data=datareg, family='binomial')
res.nosplitbestIC$evalues

evalues.df = data.frame(split=res.split$evalues, 
                        nosplit.manual = res.nosplitmanual$evalues, 
                        nonsplit.bestIC = res.nosplitbestIC$evalues)

View(res.nosplitmanual$loglik)
View(res.nosplitbestIC$loglik)

# msft = modelSelection(y~., data=datareg, family='binomial')
# coef(msft)

# glm.fit = glm(y~., data=datareg, family=binomial())
# summary(glm.fit)
