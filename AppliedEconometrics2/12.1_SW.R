#Stock & Watson Ch 12 Empirical exercise 12.1
#Fatima Sadik IBA
#Link for data: https://www.princeton.edu/~mwatson/Stock-Watson_4E/Stock-Watson-Resources-4e.html
install.packages("readxl")
install.packages("ivreg")
install.packages("stargazer")
library(readxl)
library(ivreg)
library(stargazer)
fertility <- read_excel("C:/Users/fatimasadik/OneDrive - Institute of Business Administration/Econometrics/Spring24_AE2/fertility.xlsx")

#How does fertility affect labor supply? That is, how much does a woman’s 
#labor supply fall when she has an additional child? 
#Part a
#Regress weeks worked (weeksm1) on the indicator variable morekids, using OLS. On 
#average, do women with more than two children work less than women 
#with two children? How much less?
model1<-lm(fertility$weeksm1~fertility$morekids)
summary(model1)
#Part b
#Explain why the OLS regression estimated in (a) is inappropriate for 
#estimating the causal effect of fertility (morekids) on labor supply 
#(weeksm1). More kids is endogenous. Omitted variable bias (age, edu are missing)
model2<-lm(weeksm1~morekids+agem1,data=fertility)
summary(model2)
#Inclusion of age in regression changes the coefficient of morekids.
#Part c
#The data set contains the variable samesex, which is equal to 1 if the first 
#two children are of the same sex (boy–boy or girl–girl) and equal to 0 
#otherwise. Are couples whose first two children are of the same sex more 
#likely to have a third child? Is the effect large? Is it statistically significant?
model3<-lm(morekids~samesex,data=fertility)
summary(model3)
#Couples whose first 2 children are of the same sex are on average 6.7% more likely to have more kids. The effect is
# statistically significant
#Part d
#Explain why samesexis a valid instrument for the IV regression of 
#weeksworkedon morekids
# (IV Relevance) The instrument samesex (whether the first two children are of the same sex) is correlated 
#with morekids. (IV Exogeniety) samesex is exogenous because the sex of the first two children is randomly determined and not influenced by factors that directly affect 
#weeksworked
#Part e
#Is samesex a weak IV
#Perform a regression of `morekids` on `samesex` and other exogenous controls.
#Compute the F-statistic to test the null hypothesis that `samesex` has no explanatory power for `morekids`.
#If the F-statistic is less than 10, the instrument `samesex` is considered weak.
model4<-lm(morekids~samesex+agem1+hispan+black+othrace,data=fertility)
summary(model4)
#no same sex is not a weak IV
#Part f
#Estimate the IV regression of weeksworkedon morekids, using samesex
#as an instrument. How large is the fertility effect on labor supply?
iv<-ivreg(weeksm1~morekids|samesex, data=fertility)
summary(iv)
#Part g
#nclude the variables agem1, black, 
#hispan, and othracein the labor supply regression (treating these vari-able as exogenous) and do an IV regression
iv2<-ivreg(weeksm1~morekids+agem1+hispan+black+othrace|samesex+agem1+hispan+black+othrace, data=fertility)
summary(iv2)
stargazer(iv, iv2, type = "html",
          title = "Comparison of IV Regressions",
          column.labels = c("Model 1", "Model 2"),
          dep.var.labels = "Weeks Worked",
          covariate.labels = c("More Kids", "Age Mother (agem1)", "Hispanic", "Black", "Other Race"),
          out = "iv_models_comparison.html") # Save the output to a text/html/latex file if needed