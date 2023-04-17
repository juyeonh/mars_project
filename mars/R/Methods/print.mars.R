# Print function for mars

print.mars <- function(object, ...){
  cat("Call:\n")
  print(object$call)
  cat("\nCoefficients:\n")
  cat(coefficients(object))
}
