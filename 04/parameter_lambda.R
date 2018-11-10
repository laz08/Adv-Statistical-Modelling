
# Set WD and load data
wd = getwd()
if(grepl("nora", wd)) {
    setwd("~/Documents/18-19/ASM/HW/04")
} else {
     ## Put working path for Sergi and Alex
}
rm(wd)

######
prostate <- read.table("prostate_data.txt", header=TRUE, row.names = 1)
train.sample <- which(prostate$train==TRUE)

#use.only <- 1:dim(prostate)[1]
use.only <- train.sample

Y <- scale( prostate$lpsa[use.only], center=TRUE, scale=FALSE)
X <- scale( as.matrix(prostate[use.only,1:8]), center=TRUE, scale=TRUE)
n <- dim(X)[1]
p <- dim(X)[2]

lambda.max <- 1e5
n.lambdas <- 25
lambda.v <- exp(seq(0,log(lambda.max+1),length=n.lambdas))-1

Xval <- scale(as.matrix(prostate[-use.only, 1:8]),  center=TRUE, scale=TRUE)
Yval <- scale(prostate$lpsa[-use.only], center=TRUE, scale=FALSE)


MSPEval <- function(X, Y, Xval, Yval, lambda.v) {
  PMSE.VAL <- n.lambdas
  
  # Iterate through candidate values
  for (l in 1:n.lambdas){
    lambda <- lambda.v[l]
    r <- dim(Xval)[1]
    PMSE.VAL[l] <- 0
    
    # Compute beta with the traning dataset
    beta.i <- solve(t(X)%*%X + lambda*diag(1,p)) %*% t(X) %*% Y
    for (i in 1:r){
      # Compute the errors with the validation dataset
      Xi <- Xval[i,]; Yi <- Yval[i]
      hat.Yi <- Xi %*% beta.i
      PMSE.VAL[l] <-PMSE.VAL[l] + (hat.Yi - Yi)^2
    }
    PMSE.VAL[l] <- PMSE.VAL[l]/r
  }
  return(PMSE.VAL)
}
MSPE.val = MSPEval(X, Y, Xval, Yval, lambda.v)
lambda.val <- lambda.v[which.min(MSPE.val)]

plot(log(1+lambda.v), MSPE.val)
abline(v=log(1+lambda.val),col=2,lty=2)



# Revisar
MSPEkfold <- function(X, Y, K) {
  PMSE.CV <- n.lambdas
  folds <- sample(rep(1:K, length=n), n, replace=FALSE) 
  
  # Iterate through K Folds
  for (k in 1:K){
    Xk <- as.matrix(X[folds != k,])
    Yk <- as.matrix(Y[folds != k])
    Xv <- as.matrix(X[folds == k,])
    Yv <- as.matrix(Y[folds == k])

    r <- dim(Xv)[1]
    for (l in 1:n.lambdas){
      lambda <- lambda.v[l]
      PMSE.CV[l] <- 0
      # Compute Beta with the elements in the folds
      beta.i <- solve(t(Xk)%*%Xk + lambda*diag(1,p))  %*% t(Xk) %*% Yk
      for (i in 1:r){
        #   m.Y.i <- mean(Y[-i])
        m.Y.i <- 0
        # Compute the error with the remaning elements
        Xi <- Xv[i,]; Yi <- Yv[i]
        hat.Yi <- Xi %*% beta.i + m.Y.i
        PMSE.CV[l] <- PMSE.CV[l] + (hat.Yi-Yi)^2 # Maybe not here
      }
      PMSE.CV[l] <- PMSE.CV[l]/r
    }
  }
  return(PMSE.CV)
}

mspe.10 <- MSPEkfold(X, Y, 10)
lambda.mspe.10 <- lambda.v[which.min(mspe.10)]

plot(log(1+lambda.v), mspe.10)
abline(v=log(1+lambda.mspe.10),col=2,lty=2)


mspe.5 <- MSPEkfold(X, Y, 5)
lambda.mspe.5 <- lambda.v[which.min(mspe.5)]

plot(log(1+lambda.v), mspe.5)
abline(v=log(1+lambda.mspe.5),col=2,lty=2)
