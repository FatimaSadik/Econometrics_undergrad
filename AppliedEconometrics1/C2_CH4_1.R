library(wooldridge)
data("lawsch85")
names(lawsch85)
model1<-lm(lsalary~GPA+llibvol+lcost+rank+LSAT, data=lawsch85)
summary(model1)
model2<-lm(lsalary~llibvol+lcost+rank, data=lawsch85)
summary(model2)
r2r<-summary(model2)$r.squared
r2u<-summary(model1)$r.squared
fstat<-((r2u-r2r)/2)/((1-r2u)/130)
