
# load packages (which need to be installed!)
library(lmtest)
library(car)
library(wooldridge)
data("gpa3")
# Estimate model (only for spring data)
reg <- lm(cumgpa~sat+hsperc+tothrs+female+black+white, 
          data=gpa3, subset=(spring==1))
# Usual SE:
coeftest(reg)
#hetero robust SE
coeftest(reg, vcov=hccm)
# F-Tests using different variance-covariance formulas:
myH0 <- c("black","white")
# Ususal VCOV
linearHypothesis(reg, myH0)
#BP test for hetero
bptest(reg)
#reject the null hypothesis of homoskedasticity