load("meat.Rdata")
# Only 215 obs.
# 
set.seed(123)
library(splines)

y = log(meat$Fat)
x = meat$abs.850

# sort data
sx = sort(x, index.return=T)
x = sx$x
y = y[sx$ix]

k = 3

computePredictions <- function(x, n.knots, k, y, x.val) {
  my.knots <- quantile(x$abs,seq(0,1,length=n.knots))
  l = length(my.knots)
  inner.knots <- my.knots[-c(1,l)] 
  
  df = n.knots + k + 1
  data <- cbind(x,y)
  basis <- lm(y~bs(abs, knots=inner.knots,intercept=T,degree=k, df=df), data=data)
  #lm.spl <- lm(y~basis) # remove intercept
  res <- predict(basis, x.val)
  return(res)
}


f.10.CV.inner.knots <- function(x, y, k, n.knots.range) {
  n <- length(x)
  sample <- sample(rep(1:10, length=n), n)
  k.fold.df = data.frame()
  for (ki in 1:10){
    X.train <- data.frame(abs=x[which(sample!=ki)])
    X.val <- data.frame(abs=x[which(sample==ki)])
    Y.train <- y[which(sample!=ki)]
    Y.val <- y[which(sample==ki)]
    r.sq.array = c()
    possible.knots = seq(n.knots.range[1], n.knots.range[2])
    for(n.knots in possible.knots){
      fitted.vals = computePredictions(X.train, n.knots, k, Y.train, X.val)
      (r.sq = sum(((Y.val - fitted.vals)^2)/length(Y.val)))
      r.sq.array = append(r.sq.array, r.sq)
    }
    data.f = data.frame(possible.knots, r.sq.array)
    
    minRsq = which(min(r.sq.array) == r.sq.array); minRsq = minRsq[1]
    k.fold.df = rbind(k.fold.df, c(minRsq, r.sq.array[minRsq])) 
  }
  print(k.fold.df)
  minIdx = which(min(k.fold.df[, 2]) == k.fold.df[,2])[1]
  return(k.fold.df[minIdx, 1])
}

n.knots.optim = f.10.CV.inner.knots(x, y, 3, c(1, 20))
print(n.knots.optim)
x<- data.frame(abs=x)
data <- cbind(y,x)
optim.spline <- lm(y~bs(abs, knots=n.knots.optim,intercept=T,degree=k, df=df), data=data)


plot(x$abs,y,col=2,xlab="log(Fat)",ylab="abs.850")
lines(x$abs,optim.spline$fitted.values)


degrees = n.knots.optim + k + 1

m1  <- smooth.spline(x$abs, y, df = degrees)
plot(x$abs,y, xlab="log(Fat)",ylab="abs.850")
lines(m1, col="red", lwd=2)
lines(x$abs,optim.spline$fitted.values, col="blue", lwd=2)
legend("bottomright", legend = c("Cubic Spline", "Smooth-spline"),
       lty = 1, lwd = 2,col = c("blue", "red"))
