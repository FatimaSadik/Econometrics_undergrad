library(wooldridge)
data(wage2)
names(wage2)
c(mean(wage2$wage), mean(wage2$IQ))
c(sd(wage2$wage), sd(wage2$IQ))
model1<-lm(wage~IQ, data=wage2)
summary(model1)
15*coef(model1)[2]
summary(model1)$r.squared 
model2<-lm(lwage~IQ, data=wage2)
summary(model2)
15*coef(model2)[2]
