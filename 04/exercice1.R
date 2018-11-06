prostate <- read.table("prostate_data.txt", header=TRUE, row.names = 1)



MSPEval <- function(X, y, Xval, Yval, lambda.v) {
  n.lambda <- length(lambda.v)
  for (l in 1:n.lambda){
    n <- length(Xval)
    PMSE.VAL[l] <- 0
    for (i in 1:n){
      m.Y.i <- # wut

      Xi <- Xval[i,]; Yi <- Yval[i]
      beta.i <- solve(t(Xval)%*%Xval + lambda*diag(1,p)) %*% t(Xval) %*% Yval
      hat.Yi <- Xi %*% beta.i + m.Y.i
      PMSE.VAL[l] <-PMSE[l] + (hat.Yi - Yi)^2
    }
    PMSE.VAL[l] <- PMSE[l]/n
  }
  
}