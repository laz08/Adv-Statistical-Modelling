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

# 1
m1.lasso <- glmnet(X, Y)
summary(m1.lasso)
plot(m1.lasso$df, m1.lasso$lambda)

# 2
m2.rr <- glmnet(X, Y, alpha=0)
summary(m2.rr)
plot(m2.rr$df, m2.rr$lambda)

m3.rr <- cv.glmnet(X, Y, nfolds=10)
plot(m3.rr$glmnet.fit$df, m3.rr$glmnet.fit$lambda)

m4.rr <-  MSPEkfold(X, Y, 10, n.lambdas, lambda.v, n)
lambda.CV.H.lambda <- lambda.v[which.min(m4.rr)]
