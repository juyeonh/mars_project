#' Multivariate Adaptive Regression Splines (MARS)
#'
#' Fit Friedman's Multivariate Adaptive Regression Splines (MARS) model.
#'
#' @param formula an R formula
#' @param data a data frame containing the data for the model
#' @param control an object of class 'mars.control'
#'
#' @return an object of class 'mars'
#' @export 
#'
#' @examples
#' mm <- mars(wage ~ age,data=ISLR::Wage)
#' @import stats
#' @import ISLR
#' 
#' 

mars <- function(formula,data,control=mars.control()) {
  cc <- match.call() 
  mf <- model.frame(formula,data)
  y <- model.response(mf)
  mt <- attr(mf, "terms")
  x <- model.matrix(mt, mf)[,-1,drop=FALSE]
  x_names <- colnames(x)
  control <- validate_mars.control(control)
  fwd <- fwd_stepwise(y,x,control)
  bwd <- bwd_stepwise(fwd,control)
  fit <- lm(y~.-1,data=data.frame(y=y,bwd$B)) 
  out <- c(list(call=cc,formula=formula,y=y,B=bwd$B,Bfuncs=bwd$Bfuncs,
                x_names=x_names),fit)
  class(out) <- c("mars",class(fit))
  out
}


fwd_stepwise<- function(y,x,control=mars.control()){
  N <- length(y)  # sample size
  n <- ncol(x)    # number of predictors
  
  # B: data frame with optimal basis function as columns
  B <- init_B(N, control$Mmax)    # initiate B
  Bfuncs <- vector(mode="list",length=control$Mmax+1)  # mars5 p.7: should be Mmax+1, B has Mmax+1 columns including the intercept B0
  
  for(i in 1:(control$Mmax/2)){  # mars5 p.7: loop over pairs 1 to Mmax/2
    M <- 2*i-1  # current number of basis functions to consider splitting
    lof_best <- Inf
    for(m in 1:M){     # loop over basis functions to choose a basis function to split
      vset <- setdiff(1:n, Bfuncs[[m]][,"v"])   # Vars not in B_m; returns 1:n if Bfuncs[[m]] NULL
      for (v in vset){   # Select a variable to split on
        tt <- split_points(x[,v],B[,m])
        for(t in tt){
          Bnew <- data.frame(B[,1:M],
                             # Exercise 5: replace parent B[,m] with Btem1, Btem2
                             Btem1=B[,m]*h(x[,v],1,t), 
                             Btem2=B[,m]*h(x[,v],-1,t)) 
          
          ### from Exercise 5
          gdat <- data.frame(y=y, Bnew)
          lof <- LOF(y~.-1, gdat, control)   # Lab 8 note p.3: Stop R from adding an intercept with y~.-1
          if(lof < lof_best) { 
            lof_best <- lof
            best_split <- c(m=m,v=v,t=t)
          }
        }
      }
    }
    mstar <- best_split["m"]
    vstar <- best_split["v"]
    tstar <- best_split["t"]
    
    # Updating Bfuncs and B before moving to another pair of Hinge func
    Bfuncs[[M+1]] <- rbind(Bfuncs[[mstar]], c(s=-1,vstar,tstar))
    Bfuncs[[M+2]] <- rbind(Bfuncs[[mstar]], c(s=+1,vstar,tstar))
    
    B[,M+1:2] <- cbind(B[,mstar]*h(x[,vstar],-1,tstar),
                       B[,mstar]*h(x[,vstar],+1,tstar))
  }
  colnames(B) <- paste0("B",(0:(ncol(B)-1)))
  return(list(y=y,B=B,Bfuncs=Bfuncs))    # from Lab 7
}


# From Exercise 5
init_B <- function(N,Mmax) {   
  # Input: N: # of rows
  # Mmax: # of basis funcs
  # output: a N by (Mmax+1) dataframe
  B <- data.frame( matrix(NA, nrow=N, ncol=(Mmax+1))) 
  B[,1] <- 1    # first column for intercept: B0 
  names(B) <- c("B0", paste0("B", 1:Mmax))
  return(B)
}

bwd_stepwise<-function(fwd,control){
  Mmax <- ncol(fwd$B)-1
  Jstar <- 2:(Mmax+1)    # mars6
  Kstar <- Jstar         # mars6
  dat <- data.frame(y=fwd$y,fwd$B)  # Create a data frame with response variable and basis matrix
  
  form <- as.formula(paste("y ~ ", paste(colnames(fwd$B[,Jstar]), collapse = "+")))
  lofstar <- LOF(form, dat, control)    # best LOF so far
  
  for(M in (Mmax+1):2){   # mars6 p.9 outer loop
    b <- Inf      # best LOF for inner loop
    L <- Kstar    # working copy of Kstar
    if(control$trace) cat("L: ",L, "\n")
    for (m in L){   # mars6 p.9 inner loop
      K <- setdiff(L,m)
      dat <- data.frame(y=fwd$y,fwd$B[,K])
      lof <- LOF(y~., dat, control)
      if(control$trace) cat("M=",M,", K=",K,", lof=",lof,"\n")
      
      # Update Kstar and Jstar
      # Jstar lists the set of remaining columns (predictors)
      if(lof < b){
        b <- lof
        Kstar <- K
      }
      if(lof < lofstar){
        lofstar <- lof
        Jstar <- K
      }
    }
    if(control$trace) cat("M=", M, ", Jstar=", Jstar, ", lofstar=", lofstar, "\n")
  }
  Jstar <- c(1,Jstar)
  return(list(y=fwd$y,B=fwd$B[,Jstar],Bfuncs=fwd$Bfuncs[Jstar]))
}


LOF <- function(form,data,control) {
  # RSS
  ff<-lm(form, data)
  RSS<-sum(residuals(ff)^2)
  
  # GCV (Lab 8 note p.4)
  N <- nrow(data)     # number of rows that can be obtained from the data argument
  M <- length(coef(ff)) - 1    # number of columns of the basis matrix that can be obtained by fitted model
                               # should deduct 1 from the # of coefficients
  
  CM <- sum(hatvalues(ff))
  CtildeM <- CM + control$d*M
  return (RSS*N / (N-CtildeM)^2)
}

# From Lab 7
h <- function(x,s,t) {
   return(pmax(0, s*(x-t)))
  # if x>t, s=+1, this return max(0,x-t)
  # if x<t, s=-1, this return max(0,t-x)
}

# From Exercise 5
split_points <- function(xv,Bm) {
  # input: xv: a variable xv to split
  # Bm: a parent basis func to split
  # output: feasible splitting points
  
  out <- sort(unique(xv[Bm > 0])) 
  # cat("out: ", out, "\n") 
  return(out[-length(out)])
}


#------------------------------------------------------------------------
# constructor, validator and helper for class mars.control
#------------------------------------------------------------------------

# From Lecture 7 Note p.24
new_mars.control <- function(control) {
  structure(control, class="mars.control")
}

# From Lecture 7 Note p.24
validate_mars.control <- function(control) {
  stopifnot(is.integer(control$Mmax),is.numeric(control$d),
            is.logical(control$trace))
  if(control$Mmax < 2) {
    warning("Mmax must be >= 2; Reset it to 2")
    control$Mmax <- 2}
  if(control$Mmax %% 2 > 0) {
    control$Mmax <- 2*ceiling(control$Mmax/2)
    warning("Mmax should be an even integer. Reset it to ",control$Mmax)}
  control
}


#' Constructor for `mars.control` objects
#'
#' This function constructs a `mars.control` object that specifies
#' parameters used in the model fitting procedure.
#'
#' @param Mmax Maximum number of basis functions for the forward. Should be an even integer. Default value is 2.
#' @param d Smoothing parameter of the procedure for the backward
#' @param trace If TRUE then mars function prints details of the fitting process
# 
mars.control <- function(Mmax=2,d=3,trace=FALSE) { # Given from the template
  Mmax <- as.integer(Mmax)
  control <- list(Mmax=Mmax,d=d,trace=trace)
  control <- validate_mars.control(control)
  new_mars.control(control)    # Calling constructor
}


