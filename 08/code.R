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

computeFittedValues <- function(x, n.knots, k, y) {
    my.knots <- quantile(x,seq(0,1,length=n.knots))
    l = length(my.knots)
    inner.knots <- my.knots[-c(1,l)] 
    
    df = n.knots + k + 1 
    basis <- bs(x=x,knots=inner.knots,intercept=T,degree=k, df=df)
    lm.spl <- lm(y~basis-1) # remove intercept
    return(lm.spl$fitted.values)
}


f.10.CV.inner.knots <- function(x, y, k, n.knots.range) {
  
    r.sq.array = c()
    possible.knots = seq(n.knots.range[1], n.knots.range[2])
    for(n.knots in possible.knots){
        fitted.vals = computeFittedValues(x, n.knots, k, y)
        (r.sq = sum(((y - fitted.vals)^2)/length(y)))
        r.sq.array = append(r.sq.array, r.sq)
    }
    
    data.f = data.frame(possible.knots, r.sq.array)
    print(data.f)
    
    idx = which(min(r.sq.array) == r.sq.array)
    return(possible.knots[idx])
}

n.knots.optim = f.10.CV.inner.knots(x, y, 3, c(1, 20))
fitted.vals.optim = computeFittedValues(x, n.knots.optim, k, y)
my.knots = quantile(x, seq(0,1,length=n.knots.optim))

# Plotting
plot(x,y,col=2,xlab="log( life.exp )",ylab="log( inf.mort )")
lines(x,fitted.vals.optim)
abline(v=my.knots,lty=2,col="grey")

