rm(list = ls())

# Load and install necessary packages
requiredPackages <- c("sm", "readr")

for (pac in requiredPackages) {
    if(!require(pac,  character.only=TRUE)){
        install.packages(pac, repos="http://cran.rstudio.com")
        library(pac,  character.only=TRUE)
    } 
}
rm(pac)
rm(requiredPackages)


# Set WD and load data
wd = getwd()
if(grepl("nora", wd)) {
    setwd("~/Documents/18-19/ASM/HW/07")
} else {
    # Set Sergi & Alex dirs
}
  rm(wd)

#######################
# 1./
# 
h.cv.sm.poisson <- function(x,y,rg.h=NULL,l.h=10,method=loglik.CV){
  cv.h <- numeric(l.h)
  if (is.null(rg.h)){
    hh <- c(h.select(x,y,method="cv"),
            h.select(x,y,method="aicc"))#,hcv(x,y))
    rg.h <- range(hh)*c(1/1.1, 1.5)
  }
  i <- 0
  gr.h <- exp( seq(log(rg.h[1]), log(rg.h[2]), l=l.h))
  for (h in gr.h){
    i <- i+1
    cv.h[i] <- method(x,y,h)
  }
  return(list(h = gr.h, 
              cv.h = cv.h, 
              h.cv = gr.h[which.min(cv.h)]))
}


# method loglik.CV: leave-one-out log-likelihood 
loglik.CV <- function(x,y,h){
    n <- length(x)
    pred <- sapply(1:n, 
                   function(i,x,y,h){
                       sm.poisson(x=x[-i],y=y[-i],h=h,eval.points=x[i],display="none")$estimate
                   },   x,y,h)
    total_sum = 0
    for (i in 1:n){
      total_sum = total_sum + (log((exp(-pred[i]) * ((pred[i]**y[i])/factorial(y[i])))^-i))/n
    }
    return(total_sum)
}

# 2./ 
countries <- na.omit(read_table2("countries.csv"))
summary(countries)
head(countries)
attach(countries)


le.fm.0 <- pmax(0,le.fm)

# Values we want to model to
plot(life.exp, le.fm.0)

h.CV.loglik <- h.cv.sm.poisson(life.exp,le.fm.0 ,rg.h=c(4, 30),method=loglik.CV)

plot(h.CV.loglik$h,h.CV.loglik$cv.h)
lines(h.CV.loglik$h,h.CV.loglik$cv.h)

m1 <- sm.poisson(life.exp, le.fm.0, h=h.CV.loglik$h)

