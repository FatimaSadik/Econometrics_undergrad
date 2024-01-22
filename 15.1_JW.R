#Estimating the Return to Education for Married Women
library("wooldridge")
library(AER)
data(mroz)
head(mroz)
model1<-lm(lwage~educ,data=mroz)
coefficients(model1)
summary(model1)
#we use father’s education (fatheduc) as an instrumental variable 
#for educ. We have to maintain that fatheduc is uncorrelated with u. 
#The second requirement is that educ and fatheduc are correlated.
#We can check this very easily using a simple regression of educ on fatheduc
#stage1
model2<-lm(educ~fatheduc, data=mroz)
summary(model2)
mroz$educ_hat<-fitted.values(model2)
#stage2
model3<-lm(lwage~educ_hat, data=mroz)
summary(model3)
#direct iv
iv<-ivreg(lwage~educ|fatheduc, data=mroz)
summary(iv)
coeftest(iv,vcov=vcovHC, type="HC1")
