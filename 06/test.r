library(KernSmooth)
library(sm)
library(esquisse)

data(aircraft)
attach(aircraft)

source("locpolreg.R")


lgWeight <- log(Weight)

# Look a the bandwith chooser file in the raco
bw.chooser <- function(x, y){
  dpill.bw <- dpill(x, y)
  mean(dpill.bw)
}
####################### BANDWIDTH CHOICE FUNCTIONS ####################### 
h.cv.gcv <- function(x,y,h.v = exp(seq(log(diff(range(x))/20),
                                       log(diff(range(x))/4),l=10)), 
                     p=1,type.kernel="normal"){
  n <- length(x)
  cv <- h.v*0
  gcv <- h.v*0
  for (i in (1:length(h.v))){
    h <- h.v[i]
    aux <- locpolreg(x=x,y=y,h=h,p=p,tg=x,
                     type.kernel=type.kernel, doing.plot=FALSE)
    S <- aux$S
    h.y <- aux$mtgr
    hii <- diag(S)
    av.hii <- mean(hii)
    cv[i] <- sum(((y-h.y)/(1-hii))^2)/n
    gcv[i] <- sum(((y-h.y)/(1-av.hii))^2)/n
  }
  return(list(h.v=h.v,cv=cv,gcv=gcv))
}

k.fold.cv <- function(x,y,k=10,h=range(x)/10,p=1,type.kernel="normal"){
  n <- length(x)
  Ik <- floor((0:(n-1))/(n/k))+1
  ssr <- 0
  for (i in (1:k)){
    y.i <- y[Ik==i]
    aux <- locpolreg(x[Ik!=i],y[Ik!=i],h=h,p=p,tg=x[Ik==i],
                     type.kernel=type.kernel, doing.plot=FALSE)
    ssr <- ssr + sum((y.i-aux$mtgr)^2)
  }
  k.cv <- ssr/n
  return(k.cv)
}

h.k.fold.cv <- function(x,y,h.v = exp(seq(log(diff(range(x))/20),
                                          log(diff(range(x))/4),l=10)), 
                        k=10,p=1,type.kernel="normal"){
  n <- length(x)
  perm <- sample(1:n)
  xperm <- x[perm]
  yperm <- y[perm]
  
  k.cv <- h.v*0
  for (i in (1:length(h.v))){
    h <- h.v[i]
    k.cv[i] <- k.fold.cv(x=xperm,y=yperm,k=k,h=h,p=p,
                         type.kernel=type.kernel)
  }
  return(list(k=k,h.v=h.v,k.cv=k.cv))
}

####################################################################
choose.best.bw <- function(x, y){
  cvh.bw <- h.cv.gcv(x, y)
  dpill.bw <- dpill(x, y, range.x = range(x))
  k.cv.bw <- h.k.fold.cv(x, y)
  
  cv.mean <- mean(cvh.bw[["cv"]])
  gcv.mean <- mean(cvh.bw[["gcv"]])
  kf.cv.mean <- mean(k.cv.bw[["k.cv"]])
  
  mean.bw = mean(c(cv.mean, gcv.mean, kf.cv.mean, dpill.bw))
  
  return(mean.bw)
}

plot(Yr, lgWeight)

# bw <- bw.chooser(Yr, lgWeight)
bw <- choose.best.bw(Yr, lgWeight)
m1 <- locpoly(Yr, lgWeight, bandwidth = bw, degree=1, drv=0, gridsize = length(Yr))
points(m1, col="red", type='l', lwd=3)

e <- lgWeight - m1$y
summary(e)
# Error compared to our data
plot(Yr, e^2)
z <- log(e^2)

summary(z)

z[is.infinite(z)] <- 0 # Last value is infinite so we better change it to 0

# Choose Bandwidth
bw <- choose.best.bw(Yr, z)
q <- locpoly(Yr, z,  bandwidth = bw, degree=1, drv=0, gridsize = length(Yr)) 
est.y <- exp(q$y)
est.x <- exp(q$x)
summary(est.y)
summary(est.x)
# How close is our estimator
points(Yr, est.y, col="red", type='l', lwd=3)
# How differnet is our error
plot(m1$x, m1$y - 1.96*sqrt(est.y), col="blue", type='l', lwd=2, xlim=c(15,90), ylim=c(6,12)) # Check this bc wtf is this plot maybe I understood it wrong
points(m1$x, m1$y + 1.96*sqrt(est.y), col="blue", type='l', lwd=2) # Check this bc wtf is this plot maybe I understood it wrong

points(Yr, lgWeight)
points(m1, col="red", type='l', lwd=4)
legend("topleft", c("expectation", "data", "model"), col=c("blue", "black", "red"), pch=15)

