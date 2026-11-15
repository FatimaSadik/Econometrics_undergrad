#CHAPTER 8 WLS & LPM
install.packages("wooldridge")
install.packages("sandwich")
install.packages("lmtest")
library(wooldridge)
library(sandwich)
library(lmtest)
data("gpa1")

# Generate dummy variable vector which is 1 when either the 
# father or the mother or both were at college.
gpa1$parcoll <- as.numeric(gpa1$fathcoll == 1 | gpa1$mothcoll)

LPM1<- lm(PC ~ hsGPA + ACT + parcoll, data = gpa1)

summary(LPM1)

coeftest(LPM1, vcov = vcovHC(LPM1, type = "HC0"))
hist(LPM1$fitted.values)

hhat <- LPM1$fitted.values * (1 - LPM1$fitted.values) 
# Calculate hhat (h = yhat * (1 - yhat))
LPM.wls <- lm(PC ~ hsGPA + ACT + parcoll, weights = 1 / hhat, data = gpa1)
summary(LPM.wls)
