library(wooldridge)
library(lmtest)

library(dplyr)
library(car)
library(stargazer)
data("beauty")
reg1<-lm(lwage~belavg+abvavg+female+educ+exper+expersq, data=beauty)
coeftest(reg1, vcov. = vcovHC, type = "HC1")
#regression with interactions
reg2<-lm(lwage~female*belavg+female*abvavg+female+female*educ+female*exper+female*expersq, data=beauty)
summary(reg2)
#F-test
linearHypothesis(reg2, c("female:belavg=0", "female:abvavg=0","female:educ=0","female:exper=0","female:expersq=0"))
#F-test with robust errors
linearHypothesis(reg2, c("female:belavg=0", "female:abvavg=0","female:educ=0","female:exper=0","female:expersq=0"),white.adjust = "hc1")
#stargazer(reg1,reg2)
