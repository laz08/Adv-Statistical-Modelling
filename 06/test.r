library(KernSmooth)
library(sm)

data(aircraft)
attach(aircraft)

lgWeight <- log(Weight)

# Look a the bandwith chooser file in the raco
bw.chooser <- function(x, y){
  dpill.bw <- dpill(x, y)
  mean(dpill.bw)
}

plot(Yr, lgWeight)

bw <- bw.chooser(Yr, lgWeight)
m1 <- locpoly(Yr, lgWeight, bandwidth = bw, degree=1, drv=0, gridsize = length(Yr))
points(m1, col="red", type='l', lwd=3)

e <- Yr - m1$x
summary(e)
# Error compared to our data
plot(Yr, e^2)

z <- log(e^2)
summary(z)
z[is.infinite(z)] <- 0

# Choose Bandwidth
bw <- bw.chooser(Yr, z)
q <- locpoly(Yr, z,  bandwidth = bw, degree=1, drv=0, gridsize = length(Yr)) 
est.y <- exp(q$y)
est.x <- exp(q$x)
summary(est.y)
summary(est.x)
# How close is our estimator
points(Yr, est.y, col="red", type='l', lwd=3)
# How differnet is our error
plot(m1$x, m1$y + 1.96*sqrt(est.y), col="blue", type='l', lwd=2)
points(Yr, lgWeight)
points(m1, col="red", type='l', lwd=4)
