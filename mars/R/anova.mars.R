# ANOVA function for mars

anova.mars <- function(object,...){
  cat("Analysis of Variance Table\n\nResponse: y\n")
  df <- c(rep(1, object$rank), df.residual(object))
  ssr <- sum(object$residuals^2)
  ss <- round(c(unname(object$effects[1:object$rank]^2), ssr))
  ms <- round(ss/df)
  fval <- round(ms/(ssr/df.residual(object)), digits = 2)
  fval[length(fval)] <- NA
  pr <- pf(fval, df, df.residual(object), lower.tail = FALSE)
  pr[length(pr)] <- NA
  
  cat(format("", width = nchar("residuals")), 
      format("Df", width = pmax(nchar("Df"), nchar(max(df))), justify = "right"),
      format("Sum Sq", width = pmax(nchar("Sum Sq"), nchar(max(ss))), justify = "right"),
      format("Mean Sq", width = pmax(nchar("Mean Sq"), nchar(max(ms))), justify = "right"),
      format("F Value", width = pmax(nchar("F Value"), nchar(max(fval, na.rm = TRUE))), justify = "right"),
      format("Pr(>F)", width = nchar("< 2.2e-16"), justify = "right"),
      "\n")

  for(i in object$assign) {
  cat(format(names(object$coefficients)[i], width = nchar("Residuals")), 
      format(df[i], width = pmax(nchar("Df"), nchar(max(df))), justify = "right"),
      format(ss[i], width = pmax(nchar("Sum Sq"), nchar(max(ss))), justify = "right"),
      format(ms[i], width = pmax(nchar("Mean Sq"), nchar(max(ms))), justify = "right"),
      format(fval[i], width = pmax(nchar("F Value"), nchar(max(fval, na.rm = TRUE))), justify = "right"),
      format(pr[i], width = pmax(nchar("Pr(>F)"), nchar("< 2.2e-16")), justify = "right"),
      "\n")
}
  cat(format("Residuals", width = nchar("Residuals")), 
      format(df[length(df)], width = pmax(nchar("Df"), nchar(max(df))), justify = "right"),
      format(ss[length(df)], width = pmax(nchar("Sum Sq"), nchar(max(ss))), justify = "right"),
      format(ms[length(df)], width = pmax(nchar("Mean Sq"), nchar(max(ms))), justify = "right"))
  cat("\n")
  
  # cat("Residuals\t", object$df.residual,"\t","\t","\n")
  cat("---\nSignif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n\n")
  
}
