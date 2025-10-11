library(wooldridge)
data("meap93")
names(meap93)
model1<-lm(math10~lexpend+lnchprg, data=meap93)
summary(model1)

# if expenditure increases by 1% math10 score will increase by 0.06 units on average, holding 
#other factors constant