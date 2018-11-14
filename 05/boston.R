library(MASS)
library(glmnet)
data(Boston) # TODO: Check for the Boston Corrected dataset (where to download it?)
source("all_functions.R")

set.seed(123)

lambda.max <- 1e5
n.lambdas <- 74
lambda.v <- exp(seq(0,log(lambda.max+1),length=n.lambdas))-1
## Data exploration
summary(Boston)

Y <- scale( Boston$medv, center=TRUE, scale=FALSE)
X <- scale( as.matrix(Boston[,1:13]), center=TRUE, scale=TRUE)
n <- dim(X)[1]
p <- dim(X)[2]
XtX <- t(X)%*%X 
d2 <- eigen(XtX,symmetric = TRUE, only.values = TRUE)$values
df.v <- numeric(n.lambdas)
for (l in 1:n.lambdas){
  lambda <- lambda.v[l]
  df.v[l] <- sum(d2/(d2+lambda)) 
}



# 1
m1.lasso <- glmnet(X, Y)
summary(m1.lasso)
plot(m1.lasso$df, m1.lasso$lambda, type='b')

# 2
m2.rr <- glmnet(X, Y, alpha=0)
summary(m2.rr)
plot(m2.rr)
m3.rr <- cv.glmnet(X, Y, nfolds=10, lambda = lambda.v)
plot(m3.rr)

m4.rr <-  MSPEkfold(X, Y, 10, n.lambdas, lambda.v, n)

#lambda.CV.H.lambda <- lambda.v[which.min(m4.rr)]
plot(log(m3.rr$lambda), cvm)
points(log(m3.rr$lambda), rev(m4.rr), col=5)

plot(m3.rr$glmnet.fit$df, m3.rr$cvm, col=1,pch=19,cex=.75)
points(df.v, m4.rr,col=8,pch=19,cex=.75)

