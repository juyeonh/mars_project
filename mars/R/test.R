
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
  for(i in seq(nrow(X))){
    #loop over each Bfunc
    for(j in seq(length(Bfuncs))){
      #loop over each value in an individual Bfunc
      for(k in seq(nrow(Bfuncs[[j]]))){
        B[,j] <- B[,j]*h(Bfuncs[[j]][k,"s"],X[i,Bfuncs[[j]][k,"v"]],Bfuncs[[j]][k,"t"])
        
      }
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
d <- mars(Limit ~ ., data = ISLR::Credit)
summary(d)
anova(d)
predict(d)

# Ex 3.
d <- mars(Sales ~ ., data = ISLR::Carseats)
print(d)
plot(d)
