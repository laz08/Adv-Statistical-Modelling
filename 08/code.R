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

n.knots = 10
my.knots <- quantile(x,seq(0,1,length=n.knots))
inner.knots <- my.knots[-c(1,length(my.knots))] 

basis <- bs(x=x,knots=inner.knots,intercept=T,degree=k)

dim(basis)

lm.spl <- lm(y~basis-1)
plot(x,y,col=2,xlab="log( life.exp )",ylab="log( inf.mort )")
lines(x,lm.spl$fitted.values)
abline(v=my.knots,lty=2,col="grey")

