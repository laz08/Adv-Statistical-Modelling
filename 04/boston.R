wd = getwd()
if(grepl("nora", wd)) {
    setwd("~/Documents/18-19/ASM/HW/04")
} else {
    ## Put working path for Sergi and Alex
}
rm(wd)


## Loading of libraries, data and functions
library(MASS)
data(Boston) # TODO: Check for the Boston Corrected dataset (where to download it?)
source("all_functions.R")
set.seed(123)

## Data exploration
summary(Boston)

## Base params
Y <- scale( Boston$medv, center=TRUE, scale=FALSE)
X <- scale( as.matrix(Boston[,1:13]), center=TRUE, scale=TRUE)
n <- dim(X)[1]
p <- dim(X)[2]
XtX <- t(X)%*%X 
d2 <- eigen(XtX,symmetric = TRUE, only.values = TRUE)$values

training <- n*0.8 # We take 80% of the data as training data.
Xt <- X[1:training,]
Yt <- Y[1:training,]

lambda.max <- 1e5
n.lambdas <- 25
lambda.v <- exp(seq(0,log(lambda.max+1),length=n.lambdas))-1
df.v <- numeric(n.lambdas)
for (l in 1:n.lambdas){
  lambda <- lambda.v[l]
  df.v[l] <- sum(d2/(d2+lambda)) 
}
Xval <- X[training:n,]
Yval <- Y[training:n]

beta.path <- matrix(0,nrow=n.lambdas, ncol=p)
diag.H.lambda <- matrix(0,nrow=n.lambdas, ncol=n)

for (l in 1:n.lambdas){ 
  lambda <- lambda.v[l]
  H.lambda.aux <- t(solve(XtX + lambda*diag(1,p))) %*% t(X) 
  beta.path[l,] <-  H.lambda.aux %*% Y
  H.lambda <- X %*% H.lambda.aux 
  diag.H.lambda[l,] <- diag(H.lambda)
} 


## MSPE Val
MSPE.val = MSPEval(Xt, Yt, Xval, Yval, lambda.v, n.lambdas)
lambda.val <- lambda.v[which.min(MSPE.val)]

plot(log(1+lambda.v), MSPE.val)
abline(v=log(1+lambda.val),col=2,lty=2)


## MSPE K-Fold validation
mspe.10 <- MSPEkfold(X, Y, 10, n.lambdas, lambda.v, n)
mspe.5 <- MSPEkfold(X, Y, 5, n.lambdas, lambda.v, n)

## MSPE Leave one out validation
mspe.cv <- MSPEcv(X, Y, n.lambdas, lambda.v, n)

lambda.CV <- lambda.v[which.min(mspe.cv)]
plot(log(1+lambda.v), mspe.cv)
abline(v=log(1+lambda.CV),col=2,lty=2)

## MSPE GCV
mspe.gcv <- MSPEgcv(X, Y, n.lambdas, lambda.v, beta.path, diag.H.lambda, n)
df.GCV <- df.v[which.min(mspe.gcv)]

## MSPE cv Diag H
mspe.d.h <- MSPEcvDiagH(X, Y, n, n.lambdas, lambda.v, diag.H.lambda, beta.path)
lambda.CV.H.lambda <- lambda.v[which.min(mspe.d.h)]
df.CV.H.lambda <- df.v[which.min(mspe.d.h)]

## Plotting
plot(df.v, MSPE.val)
points(df.v, mspe.cv,col=13,pch=19,cex=.75)
points(df.v, mspe.5,col=12,pch=19,cex=.75)
points(df.v, mspe.10,col=8,pch=19,cex=.75)
points(df.v, mspe.gcv,col=10,pch=2,cex=1.5)

abline(v=df.GCV,col=1,lty=2,lwd=2)
abline(v=0.001,col=2,lty=2,lwd=1, cex=0.35) # Since df.CV.H.lambda == 0, this is to force the print of this abline
# abline(v=df.CV.H.lambda + 0.001,col=2,lty=2, lwd=3)

legend("top",
       c("MSPE.val","MSPE.CV", "MSPE 5-Fold", "MSPE 10-Fold", "MSPE GCV", "lambda.GCV","lambda.CV"),
       pch=c(2, 19, 19, 19, 2, NA, NA),
       lty=c(0,0,0,0,0,2,2),
       lwd=c(0,0,0,0,0,2,1),
       col=c(9, 13, 12, 8, 10, 1, 2)
       )


