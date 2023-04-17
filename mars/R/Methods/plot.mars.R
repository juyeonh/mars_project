# Plot function for mars

plot.mars <- function(object){
  CDF <- ecdf(abs(object$residuals))
  
  par(mfrow=c(3,1))
  
  plot(CDF)
  plot(lm(object$y~.-1,data=data.frame(y=object$y, object$B)))
  qqnorm(object$residuals, pch = 1)
  qqline(object$residuals, col = "red")
}




# Using ANOVA..?
a <- aov(wage ~ age,data=ISLR::Wage)
summary(a)
plot(a)





# Cumulative dist.
CDF <- ecdf(abs(mm$residuals))
plot(CDF)

# linear regression
plot(lm(mm$y~.-1,data=data.frame(y=mm$y, mm$B)))

# QQ
qqnorm(mm$residuals, pch = 1)
qqline(mm$residuals, col = "red")

