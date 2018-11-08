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

# I think its done
MSPEval <- function(X, Y, Xval, Yval, lambda.v) {
  PMSE.VAL <- numeric(n.lambdas)
  for (l in 1:n.lambdas){
    lambda <- lambda.v[l]
    r <- dim(Xval)[1]
    PMSE.VAL[l] <- 0
    beta.i <- solve(t(X)%*%X + lambda*diag(1,p)) %*% t(X) %*% Y
    for (i in 1:r){
      m.Y.i <- 0# wut
      
      Xi <- Xval[i,]; Yi <- Yval[i]
      hat.Yi <- Xi %*% beta.i + m.Y.i
      PMSE.VAL[l] <-PMSE.VAL[l] + (hat.Yi - Yi)^2
    }
    PMSE.VAL[l] <- PMSE.VAL[l]/n
  }
  return(PMSE.VAL)
}
MSPEval(X, Y, Xval, Yval, lambda.v)

# Revisar
MSPEkfold <- function(X, y, K) {
  PMSE.CV <- numeric(n.lambdas)
  folds <- sample(rep(1:K, length=n), n, replace=FALSE) 
  for (k in 1:K){
    
    Xk <- as.matrix(X[folds != k,])
    Yk <- Y[folds != k]
    for (l in 1:n.lambdas){
      lambda <- lambda.v[l]
      PMSE.CV[l] <- 0
      for (i in 1:length(Xk)){
        #   m.Y.i <- mean(Y[-i])
        m.Y.i <- 0
        Xi <- Xk[i,]; Yi <- Y[i]
        beta.i <- solve(t(Xk)%*%Xk + lambda*diag(1,p))  %*% Yk
        hat.Yi <- Xi %*% beta.i
        PMSE.CV[l] <- PMSE.CV[l] + (hat.Yi-Yi)^2 # Maybe not here
      }
      PMSE.CV[l] <- PMSE.CV[l]/n
    }
  }
}

MSPEval(Xt, Yt, Xval, Yval, lambda.v)
#MSPEkfold(X, Y, 10)
