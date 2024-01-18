install.packages("AER")
#install.packages("stargazer")
library(AER)
#library(stargazer)
data("HMDA")
head(HMDA)
HMDA$deny <- as.numeric(HMDA$deny)-1
# estimate a simple linear probabilty model
denymod1 <- lm(deny ~ pirat, data = HMDA)
denymod1
#The model indicates that there is a positive relation between the payment-to-income ratio and the probability of a denied mortgage application so
#individuals with a high ratio of loan payments to income are more likely to be rejected.

coeftest(denymod1, vcov. = vcovHC, type = "HC1")

#The coefficient on P/I ratio is statistically different from 0 at the 1% level. Its estimate can be
#interpreted as follows: a 1 percentage point increase in P/I ratio leads to an increase in the probability
#of a loan denial by 0.604 · 0.01 = 0.00604 ≈ 0.6%.
hist(denymod1$fitted.values)

# rename the variable 'afam' for consistency
colnames(HMDA)[colnames(HMDA) == "afam"] <- "black"
#estimate the model
denymod2 <- lm(deny ~ pirat + black, data = HMDA)
coeftest(denymod2, vcov. = vcovHC)
stargazer(denymod1)
#The coefficient on black is positive and significantly different from zero at the 0.01% level. The
#interpretation is that, holding constant the P/I ratio, being black increases the probability of a
#mortgage application denial by about 17.7%.