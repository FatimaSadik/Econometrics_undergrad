install.packages("remotes")
remotes::install_github("ccolonescu/PoEdata")
library(PoEdata)
library(stargazer)
data("coke")
head(coke)
lpm<-lm(coke~pratio+disp_coke+disp_pepsi,data=coke)
summary(lpm)
hist(fitted.values(lpm))
#negative predicted probabilities 
?glm
logit<-glm(coke~pratio+disp_coke+disp_pepsi, data=coke, family =binomial(link="logit"))
summary(logit)
# Get coefficient estimates
coefficients <- coef(logit)

# Calculate odds ratios
odds_ratios <- exp(coefficients)

# Print or display the odds ratios
print(odds_ratios)


#probit model
probit<-glm(coke~pratio+disp_coke+disp_pepsi, data=coke, family =binomial(link="probit"))
summary(probit)
stargazer(lpm,logit,probit, type="text", out="all.htm")

#predicted probability

allmean <- data.frame(pratio=mean(coke$pratio),
                      disp_pepsi=1,
                      disp_coke=1)
allmean
allmean$pred.prob <- predict(logit, newdata=allmean, type="response")
allmean

#marginal effects
install.packages("mfx")

library(mfx)
logitmfx(coke~pratio+disp_coke+disp_pepsi, data=coke)
