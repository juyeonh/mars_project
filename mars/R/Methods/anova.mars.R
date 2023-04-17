# ANOVA function for mars

anova.mars <- function(object,...){
  cat("Analysis of Variance Table\n\nResponse: y\n")
  cat("\tDf\tSum Sq\tMean Sq\tF value\tPr(>F)\n")
  for(i in 1:length(object$Bfuncs)){
    cat(names(object$coefficients)[i],"\t")
    
    ######## Didn't finish yet:(
    cat("\t",)                 # sum sq
    cat(,"\t")                 # mean sq
    fvalue <-                  # F value
    cat(fvalue,"\t")           # Pr(>F)
    f <- 
    ########
    if(f < 2.2e-16){
      cat("<2.2e-16   ")
    }else{
      cat(f,"   ")
    }
    if(f < 0.001){
      cat("***")
    } else if(f < 0.01){
      cat("**")
    } else if(f < 0.05){
      cat("*")
    } else if(f < 0.1){
      cat(".")
    }
    cat("\n")
  }
  # cat("Residuals\t", object$df.residual,"\t","\t","\n")
  cat("---\nSignif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n\n")
  
}