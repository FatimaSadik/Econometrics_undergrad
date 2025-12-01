library(wooldridge)
data("vote1")
names(vote1)
model1<-lm(voteA~prtystrA+democA+lexpendA+lexpendB,data=vote1)
vote1$uhat<-model1$residuals
vote1$uhat2<-(vote1$uhat)^2
model2<-lm(uhat~prtystrA+democA+lexpendA+lexpendB,data=vote1)
summary(model2)$r.squared
#explanatory variables are independent of the residuals corr(x,uhat)=0
#BP Test
bp<-lm(uhat2~prtystrA+democA+lexpendA+lexpendB,data=vote1)
summary(bp)
#White's Test
vote1$yhat<-model1$fitted.values
vote1$yhat2<-(vote1$yhat)^2
white<-lm(uhat2~yhat+yhat2,data=vote1)
summary(white)
