
# Set WD and load data
wd = getwd()
if(grepl("nora", wd)) {
    setwd("~/Documents/18-19/ASM/HW/04")
} else {
     ## Put working path for Sergi and Alex
}
rm(wd)

######
source("all_functions.R")
set.seed(123)
######

prostate <- read.table("prostate_data.txt", header=TRUE, row.names = 1)
train.sample <- which(prostate$train==TRUE)

#use.only <- 1:dim(prostate)[1]
use.only <- train.sample

Y <- scale( prostate$lpsa[use.only], center=TRUE, scale=FALSE)
X <- scale( as.matrix(prostate[use.only,1:8]), center=TRUE, scale=TRUE)
n <- dim(X)[1]
p <- dim(X)[2]
XtX <- t(X)%*%X 
d2 <- eigen(XtX,symmetric = TRUE, only.values = TRUE)$values

lambda.max <- 1e5
n.lambdas <- 25
lambda.v <- exp(seq(0,log(lambda.max+1),length=n.lambdas))-1

Xval <- scale(as.matrix(prostate[-use.only, 1:8]),  center=TRUE, scale=TRUE)
Yval <- scale(prostate$lpsa[-use.only], center=TRUE, scale=FALSE)

beta.path <- matrix(0,nrow=n.lambdas, ncol=p)
diag.H.lambda <- matrix(0,nrow=n.lambdas, ncol=n)

for (l in 1:n.lambdas){ 
    lambda <- lambda.v[l]
    H.lambda.aux <- t(solve(XtX + lambda*diag(1,p))) %*% t(X) 
    beta.path[l,] <-  H.lambda.aux %*% Y
    H.lambda <- X %*% H.lambda.aux 
    diag.H.lambda[l,] <- diag(H.lambda)
} 


df.v <- numeric(n.lambdas)
for (l in 1:n.lambdas){
    lambda <- lambda.v[l]
    df.v[l] <- sum(d2/(d2+lambda)) 
}

MSPE.val = MSPEval(X, Y, Xval, Yval, lambda.v, n.lambdas)
lambda.val <- lambda.v[which.min(MSPE.val)]

plot(log(1+lambda.v), MSPE.val)
abline(v=log(1+lambda.val),col=2,lty=2)


mspe.10 <- MSPEkfold(X, Y, 10, n.lambdas, lambda.v, n)
lambda.mspe.10 <- lambda.v[which.min(mspe.10)]

plot(log(1+lambda.v), mspe.10)
abline(v=log(1+lambda.mspe.10),col=2,lty=2)


mspe.5 <- MSPEkfold(X, Y, 5, n.lambdas, lambda.v, n)
lambda.mspe.5 <- lambda.v[which.min(mspe.5)]

plot(log(1+lambda.v), mspe.5)
abline(v=log(1+lambda.mspe.5),col=2,lty=2)
