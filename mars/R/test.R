
predict.mars <- function(object, newdata) {
  if(missing(newdata) || is.null(newdata)) {
    B <- as.matrix(object$B)
  }
  else {
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

h <- function(x,s,t) {
  return(pmax(0, s*(x-t)))
  # if x>t, s=+1, this return max(0,x-t)
  # if x<t, s=-1, this return max(0,t-x)
}

make_B <- function(X, Bfuncs) {
  #initialize matrix
  B<-matrix(1, nrow = nrow(X), ncol = length(Bfuncs))
  #loop over each row of X
  for(i in seq_len(nrow(X))){
    #loop over each Bfunc
    for(j in seq_len(length(Bfuncs))){
      temp<-1
      hnum<-length(Bfuncs[[j]][,1])
      #loop over each value in an individual Bfunc
      for(k in seq_len(hnum)){
        temp<-temp*h(Bfuncs[[j]][k,1],X[i,Bfuncs[[j]][k,2]],Bfuncs[[j]][k,3])
      }
      B[i,j]<-B[i,j]*temp
    }
  }
  return(B)
}

# Ex 1.
dir <- ifelse(.Platform$OS.type=="unix",
              "/Users/santiago/Library/CloudStorage/GoogleDrive-santi9608@gmail.com/My Drive/School/SFU/Upper Division/STAT360",
              "C:/Users/guerr/Google Drive/School/SFU/Upper Division/STAT360")
load(paste0(dir, "/Project/marstestdata.rda"))
load(paste0(dir, "/Exercises/ProjectTestfiles/testthat/testmars.RData"))
load(paste0(dir, "/Exercises/ProjectTestfiles/testthat/testpredict.RData"))
predict.mars(testmars)
predict.mars(testmars, marstestdata)
all.equal(predict.mars(testmars), testpredict)

# Ex 2.

# Ex 3.