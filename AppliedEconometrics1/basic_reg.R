data(attend, package='wooldridge')

# Estimate model with interaction effect:
reg1<-lm(stndfnl~atndrte*priGPA+ACT+I(priGPA^2)+I(ACT^2), data=attend)
reg1
summary(reg1)
#coefficients[1]
# Estimate for partial effect at priGPA=2.59:
b <- coef(reg1)
b
b["atndrte"] + 2.59*b["atndrte:priGPA"] 

# Test partial effect for priGPA=2.59:
library(car)
linearHypothesis(myres,c("atndrte+2.59*atndrte:priGPA"))