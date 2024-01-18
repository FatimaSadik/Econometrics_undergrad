library(lmtest)
library(car)
library(wooldridge)
data("hprice1")

reg <- lm(price~lotsize+sqrft+bdrms, data=hprice1)
summary(reg)
bptest(reg)
reglog <- lm(log(price)~log(lotsize)+log(sqrft)+bdrms, data=hprice1)
summary(reglog)

#B-P test
bptest(reglog)