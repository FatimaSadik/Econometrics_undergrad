library(wooldridge)
library(car)
data("vote1")
names(vote1)
model1<-lm(voteA~log(expendA)+log(expendB)+prtystrA,data=vote1)
summary(model1)
linearHypothesis(model1,"log(expendA)+log(expendB)=0")

b1<-coef(model1)[2]
b2<-coef(model1)[3]
b1_b2<-b1+b2
varb1<-vcov(model1)[2,2]
varb2<-vcov(model1)[3,3]
covb1b2<-vcov(model1)[2,3]
stat<-b1_b2/sqrt(varb1+varb2+2*covb1b2)
stat


