#Stock & Watson Ch 12 Empirical exercise 12.1
#Fatima Sadik IBA
#Link for data: https://www.princeton.edu/~mwatson/Stock-Watson_4E/Stock-Watson-Resources-4e.html
library(readxl)
fertility <- read_excel("C:/Users/fatimasadik/OneDrive - Institute of Business Administration/Econometrics/Spring24_AE2/fertility.xlsx")
#Part a
model1<-lm(fertility$weeksm1~fertility$morekids)
summary(model1)
#Part b
model2<-lm(weeksm1~morekids+agem1,data=fertility)
summary(model2)
#Inclusion of age in regression changes the coefficient of morekids.
#Part c
model3<-lm(morekids~samesex,data=fertility)
summary(model3)
#Part f
iv<-ivreg(weeksm1~morekids|samesex, data=fertility)
summary(iv)
#Part g
iv2<-ivreg(weeksm1~morekids+agem1+hispan+black+othrace|samesex+agem1+hispan+black+othrace, data=fertility)
summary(iv2)
