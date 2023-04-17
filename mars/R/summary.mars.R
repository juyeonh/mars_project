# Summary function for mars

summary.mars <- function(object,...){
  cat("Call:\n")
  print(object$call)
  
  cat("\nResiduals:\n")
  cat("      Min       1Q     Median       3Q      Max\n")
  cat(quantile(residuals(object)))
  
  cat("\n\nCoefficients:\n")
  cat("\tEstimate\tStd. Error\tt value \tPr(>|t|)\n")
  for(i in 1:length(object$Bfuncs)){
    cat(names(object$coefficients)[i],"\t")
    cat(coef(object)[[i]])
    cat("\t",sqrt(diag(vcov(object)))[i],"\t")
    tvalue <- coef(object)[[i]] / sqrt(diag(vcov(object)))[i]
    cat(tvalue,"\t")
    t <- 2 * pt(abs(tvalue), object$df.residual, lower.tail = FALSE)
    if(t < 2e-16){
      cat("<2e-16   ")
    }else{
      cat(t,"   ")
    }
    if(t < 0.001){
      cat("***")
    } else if(t < 0.01){
      cat("**")
    } else if(t < 0.05){
      cat("*")
    } else if(t < 0.1){
      cat(".")
    }
    cat("\n")
  }
  cat("---\nSignif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n\n")
  cat("Residual standard error: ", sqrt(deviance(object)/df.residual(object)), " on ", df.residual(object) , " degrees of freedom\n")
}
