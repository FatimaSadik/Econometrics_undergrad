library(wooldridge)
library(dplyr)
data("countymurders")
df<-countymurders %>%
  filter(year==1996)
#How many counties had zero murders in 1996?
zero_murders<-sum(df$murders==0)
#How many counties had at least one execution?
executions<-sum(df$execs>=1)
#What is the largest number of executions?
max(df$execs)
#reg model 
model1<-lm(murders~execs, data=df)
summary(model1)
summary(model1)$r.squared 
nrow(df)
# least predicted murders
min(model1$fitted.values)
## residual for county with execs=0, murders=0
uhat_0<--model1$coefficients[1]
uhat_0<--predict(model1, newdata = data.frame(execs = 0))

#1.Omitted variables bias
#2.Reverse causality
#3.Discrete nature of data
