load("meat.Rdata")
# Only 215 obs.
# 
library(splines)

y = log(meat$Fat)
x = meat$abs.850

# sort data
sx = sort(x, index.return=T)
x = sx$x
y = y[sx$ix]

k = 3   #Todo: check what happens if degree =1

computeFittedValues <- function(x, n.knots, k, y, x.val) {
  my.knots <- quantile(x,seq(0,1,length=n.knots))
  l = length(my.knots)
  inner.knots <- my.knots[-c(1,l)] 
  
  df = n.knots + k + 1 
  basis <- bs(x=x,knots=inner.knots,intercept=T,degree=k, df=df)
  lm.spl <- lm(y~basis-1) # remove intercept
  print(lm.spl)
  #basis1 <- bs(x=x.val,knots=inner.knots,intercept=T,degree=k, df=df)
  
  res <- predict(lm.spl, x.val)
  return(res)
}


f.10.CV.inner.knots <- function(x, y, k, n.knots.range) {
  n <- length(x)
  sample <- sample(rep(1:10, length=n), n)
  k.fold.df = data.frame()
  for (ki in 1:10){
    X.train <- x[which(sample!=ki)]
    X.val <- data.frame(x = x[which(sample==ki)])   
    Y.train <- y[which(sample!=ki)]
    Y.val <- y[which(sample==ki)]
    
    r.sq.array = c()
    possible.knots = seq(n.knots.range[1], n.knots.range[2])
    for(n.knots in possible.knots){
      fitted.vals = computeFittedValues(X.train, n.knots, k, Y.train, X.val)
      (r.sq = sum(((Y.val - fitted.vals)^2)/length(Y.val)))
      r.sq.array = append(r.sq.array, r.sq)
    }
    
    data.f = data.frame(possible.knots, r.sq.array)
    if(k == 1){
      print(data.f)
      print(which(min(r.sq.array) == r.sq.array)[1])
    }
    minRsq = which(min(r.sq.array) == r.sq.array); minRsq = minRsq[1]
    k.fold.df = rbind(k.fold.df, c(minRsq, r.sq.array[minRsq])) 
  }
  print(k.fold.df)
  minIdx = which(min(k.fold.df[, 1]) == k.fold.df[,1])[1]
  return(k.fold.df[minIdx, 1])
}
n.knots.optim = f.10.CV.inner.knots(x, y, 3, c(1, 20))
print(n.knots.optim)
fitted.vals.optim = computeFittedValues(x, n.knots.optim, k, y)
my.knots = quantile(x, seq(0,1,length=n.knots.optim))




# task 2
n.knots = 10
my.knots <- quantile(x,seq(0,1,length=n.knots))
inner.knots <- my.knots[-c(1,length(my.knots))] 


degrees <- n.knots + k + 1

basis <- bs(x=x,knots=inner.knots,intercept=T,degree=k, df=degrees)

dim(basis)

lm.spl <- lm(y~basis)



plot(x,y,col=2,xlab="log( life.exp )",ylab="log( inf.mort )")
lines(x,fitted.vals.optim)
abline(v=my.knots,lty=2,col="grey")

sum((y - lm.spl$fitted.values)^2)/length(y)


m1  <- smooth.spline(x, y, df = degrees)
plot(x,y)
lines(m1, col="red")
lines(x,lm.spl$fitted.values, col="blue")

