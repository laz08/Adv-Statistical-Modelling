
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