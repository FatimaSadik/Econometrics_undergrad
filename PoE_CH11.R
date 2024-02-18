install.packages("systemfit")
install.packages("remotes")
remotes::install_github("ccolonescu/PoEdata")
library(systemfit)
library(PoEdata)
data("truffles")
#reduced form equations/first stage
rf_p<-lm(p~ps+di+pf,data=truffles)
rf_q<-lm(q~ps+di+pf,data=truffles)
summary(rf_p)
summary(rf_q)
truffles$p_hat<-fitted.values(rf_p)
truffles$q_hat<-fitted.values(rf_q)
#2nd stage
seq_1<-lm(q~p_hat+ps+di,data=truffles)
summary(seq_1)
seq_2<-lm(q~p_hat+pf,data=truffles)
summary(seq_2)
#ALTERNATIVELY

D <- q~p+ps+di
S <- q~p+pf
sys <- list(D,S)
instr <- ~ps+di+pf
truff.sys <- systemfit(sys, inst=instr,
                       method="2SLS", data=truffles)
summary(truff.sys)
