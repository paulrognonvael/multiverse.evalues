library(mvtnorm)
n=10000
p=20
x =rmvnorm(n, mean=rep(0,p))
beta = c(runif(2,1/2,2/3),1:5,rep(0,13))
y = 1/(1+exp(-x %*% beta + rnorm(n,sd=0.5))) > 0.5
datareg = data.frame(y=y)
datareg = cbind(datareg, x)


tt = evalues(formula=y~., data=datareg, family='binomial')
tt$evalues

msft = modelSelection(y~., data=datareg, family='binomial')
coef(msft)
