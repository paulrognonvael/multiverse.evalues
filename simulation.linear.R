library(mvtnorm)
n=5000
p=20
x =rmvnorm(n, mean=rep(0,p))
beta = c(runif(2,1/2,2/3),1:5,rep(0,13))


#### Linear regression 
y = x %*% beta + rnorm(n,sd=0.5)
datareg = data.frame(y=y)
datareg = cbind(datareg, x)


#res.split = evalues.split(formula=y~., data=datareg, family='normal')
#res.split$evalues

res.nosplitmanual = evalues.nosplit(formula=y~., data=datareg, family='normal')
res.nosplitmanual$evalues

res.nosplitbestIC = evalues.nosplit2(formula=y~., data=datareg, family='normal')
res.nosplitbestIC$evalues

evalues.df = data.frame(split=res.split$evalues, 
                        nosplit.manual = res.nosplitmanual$evalues, 
                        nonsplit.bestIC = res.nosplitbestIC$evalues)

View(res.nosplitmanual$loglik)
View(res.nosplitbestIC$loglik)

msft = modelSelection(y~., data=datareg, family='normal')
coef(msft)
