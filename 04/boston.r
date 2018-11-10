library(MASS)

data(Boston)
summary(Boston)

Y <- scale( Boston$medv, center=TRUE, scale=FALSE)
X <- scale( as.matrix(Boston[,1:13]), center=TRUE, scale=TRUE)
n <- dim(X)[1]
p <- dim(X)[2]
XtX <- t(X)%*%X 
d2 <- eigen(XtX,symmetric = TRUE, only.values = TRUE)$values

traning <- n*0.8 
Xt <- X[1:traning,]
Yt <- Y[1:traning,]

lambda.max <- 1e5
n.lambdas <- 25
lambda.v <- exp(seq(0,log(lambda.max+1),length=n.lambdas))-1
df.v <- numeric(n.lambdas)
for (l in 1:n.lambdas){
  lambda <- lambda.v[l]
  df.v[l] <- sum(d2/(d2+lambda)) 
}
Xval <- X[traning:n,]
Yval <- Y[traning:n]

beta.path <- matrix(0,nrow=n.lambdas, ncol=p)
diag.H.lambda <- matrix(0,nrow=n.lambdas, ncol=n)
for (l in 1:n.lambdas){ 
  lambda <- lambda.v[l]
  H.lambda.aux <- t(solve(XtX + lambda*diag(1,p))) %*% t(X) 
  beta.path[l,] <-  H.lambda.aux %*% Y
  H.lambda <- X %*% H.lambda.aux 
  diag.H.lambda[l,] <- diag(H.lambda)
} 

# Validation
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
MSPE.val = MSPEval(Xt, Yt, Xval, Yval, lambda.v)
lambda.val <- lambda.v[which.min(MSPE.val)]

plot(log(1+lambda.v), MSPE.val)
abline(v=log(1+lambda.val),col=2,lty=2)



# K fold
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
mspe.5 <- MSPEkfold(X, Y, 5)


# leave-one-out
MSPEcv <- function(X, Y) {
  PMSE.CV <- numeric(n.lambdas)
  for (l in 1:n.lambdas){
    lambda <- lambda.v[l]
    PMSE.CV[l] <- 0
    for (i in 1:n){
      #   m.Y.i <- mean(Y[-i])
      m.Y.i <- 0
      X.i <- X[-i,]; Y.i <- Y[-i]-m.Y.i
      Xi <- X[i,]; Yi <- Y[i]
      beta.i <- solve(t(X.i)%*%X.i + lambda*diag(1,p)) %*% t(X.i) %*% Y.i
      hat.Yi <- Xi %*% beta.i + m.Y.i
      PMSE.CV[l] <- PMSE.CV[l] + (hat.Yi-Yi)^2
    }
    PMSE.CV[l] <- PMSE.CV[l]/n
  }
  return(PMSE.CV)
}

mspe.cv <- MSPEcv(X, Y)


lambda.CV <- lambda.v[which.min(mspe.cv)]
plot(log(1+lambda.v), mspe.cv)
abline(v=log(1+lambda.CV),col=2,lty=2)

MSPEgcv <- function(X, Y) {
  PMSE.GCV <- numeric(n.lambdas)
  for (l in 1:n.lambdas){
    lambda <- lambda.v[l]
    hat.Y <- X %*% beta.path[l,]
    nu <- sum(diag.H.lambda[l,])
    PMSE.GCV[l] <- sum( ((Y-hat.Y)/(1-nu/n))^2 )/n
  }
  return(PMSE.GCV)
}

mspe.gcv <- MSPEgcv(X, Y)


plot(df.v, MSPE.val)
points(df.v, mspe.cv,col=6,pch=19,cex=.75)
points(df.v, mspe.5,col=7,pch=19,cex=.75)
points(df.v, mspe.10,col=8,pch=19,cex=.75)
points(df.v, mspe.gcv,col=10,pch=19,cex=.75)

#abline(v=df.GCV,col=1,lty=2,lwd=3)
#abline(v=df.CV.H.lambda,col=6,lty=6)
legend("top",c("PMSE.GCV","PMSE.CV","lambda.GCV","lambda.CV"),
       pch=c(1,19,NA,NA),lty=c(0,0,2,6),lwd=c(0,0,3,1),col=c(1,6,1,6))

