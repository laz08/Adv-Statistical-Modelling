library(sm)

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
  return(-sum( y*log(pred/(1-pred)) + log(1-pred) )/n)
}