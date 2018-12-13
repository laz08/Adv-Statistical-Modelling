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
m1.lasso <- glmnet(X, Y, lambda= lambda.v)
summary(m1.lasso)
plot(m1.lasso,  xvar="lambda"))

plot(log(m1.lasso$lambda), m1.lasso$dev.ratio, type='b')
abline(v=log(min(m1.lasso$lambda)),col=2,lty=2)
# 2
m2.rr <- glmnet(X, Y, alpha=0)
summary(m2.rr)
plot(m2.rr,  xvar="lambda"))
plot(log(m2.rr$lambda), m2.rr$dev.ratio, type='b')


m3.rr <- cv.glmnet(X, Y, nfolds=10, lambda =  lambda.v, standardize=FALSE, intercept=FALSE, alpha=0)
plot(m3.rr, col=2)
abline(v=log(m3.rr$lambda.min),col=2,lty=2)
abline(v=log(m3.rr$lambda.1se),col=2,lty=2)
cvm <- rev(m3.rr$cvm)
m4.rr <-  MSPEkfold(X, Y, 10, n.lambdas, lambda.v, n)

points(log(lambda.v), m4.rr, col=5)
legend("bottomright", c("glmnet error", "k.fold error"), col=c(2,5), lty=c(1,1), lwd=c(0,0))

lambda.glm <- m3.rr$lambda
df.v.g <- numeric(n.lambdas)
for (l in 1:n.lambdas){
  lambda <- lambda.glm[l]
  df.v.g[l] <- sum(d2/(d2+lambda)) 
}
plot(df.v, cvm, col=1,pch=19,cex=.75)
points(df.v, m4.rr,col=8,pch=19,cex=.75)

