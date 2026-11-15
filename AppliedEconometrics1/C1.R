install.packages("wooldridge")
library(wooldridge)
#Fatima Sadik
#Computer Exercise 1 Wooldridge Ch8
data("sleep75")
OLS<-lm(sleep~totwrk+educ+age+agesq+yngkid+male,data=sleep75)
summary(OLS)
uhat<-OLS$residuals
uhat2<-uhat^2
df<-data.frame(uhat2,sleep75$male)
uhatsq.male<-mean(subset(df, sleep75.male == 1)$uhat2)
uhatsq.female<-mean(subset(df, sleep75.male == 0)$uhat2)      
summary(lm(uhat2~sleep75.male,data=df))
summary(lm(uhat2~sleep75$male+sleep75$totwrk+sleep75$educ+sleep75$age+sleep75$agesq+sleep75$yngkid))
#fail to reject the null