# predict function for mars

predict.mars <- function(object,newdata) { 
  if(missing(newdata) || is.null(newdata)) {
    B <- as.matrix(object$B) 
  } else {
    tt <- terms(object$formula,data=newdata)
    tt <- delete.response(tt)
    mf <- model.frame(tt,newdata)
    mt <- attr(mf, "terms")
    X <- model.matrix(mt, mf)[,-1] # remove intercept 
    B <- make_B(X,object$Bfuncs)
  }
  beta <- object$coefficients 
  drop(B %*% beta)
}

# required to use hinge function (h) on make_B()
h <- function(x,s,t) {
  return(pmax(0,s*(x-t)))
}


make_B <- function(X, Bfuncs){
  B <- matrix(1, nrow=nrow(X), ncol=length(Bfuncs))
  for(i in 1:nrow(X)){
    for(j in 1:length(Bfuncs)){
      for(k in 1:nrow(Bfuncs[[j]])){
        B[,j] <- B[,j]*h(Bfuncs[[j]][k,"s"],X[i,Bfuncs[[j]][k,"v"]],Bfuncs[[j]][k,"t"])
      }
    }
  }
  return(B)
}

