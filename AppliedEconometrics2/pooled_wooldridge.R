library(wooldridge)
data("fertil1")
#After controlling for other observable factors, what has happened to
#fertility rates over time?
fertil1$year<-factor(fertil1$year)
model1<-lm(kids~educ+year+age+agesq+black+east+northcen+west+farm+othrural+town+smcity, data=fertil1)
coeftest(model1, vcov. = vcovHC, type = "HC1")
#the coefficient on y82 implies that, holding education, age, and
#other factors fixed, a woman had on average .52 less children, or about one-half a child, in 1982 than
#in 1972. This is a very large drop: holding educ, age, and the other factors fixed, 100 women in 1982
#are predicted to have about 52 fewer children than 100 comparable women in 1972.

