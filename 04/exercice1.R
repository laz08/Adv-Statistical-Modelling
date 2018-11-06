prostate <- read.table("prostate_data.txt", header=TRUE, row.names = 1)



MSPEval <- function(X, y, Xval, Yval, lambda.v) {
  n.lambda <- length(lambda.v)

  for (l in 1:n.lambda){
    # m.Y.i <- 0 # Why tho , maybe we need to use the lm and calculate mean?
    for (i in 1:n){
      err <- 
    }
}