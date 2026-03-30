install.packages("remotes")
install.packages("nnet")
install.packages("MASS")
remotes::install_github("ccolonescu/PoEdata")
library(PoEdata)
library(nnet)
library("MASS")
data("nels_small")
head(nels_small)
model1<-multinom(psechoice~grades,data=nels_small)
summary(model1)
#the group that did not attend college is the base group, so b11=b21=0 
#b12=2.506 and b22=-0.308
#b13=5.77 and b23=-0.706
#A larger value of grades means poorer academic performance. If the value of grades increases
# the probability of choosing 2 and 4 year college decreases as compared to no-college.

model2<-multinom(psechoice~grades+female,data=nels_small)
summary(model2)
#females are more likely to choose a 2 year college over no-college as compared to males
#females are less likely to choose a 4 year college over no-college as compared to males
nels_small$psechoice1<-factor(nels_small$psechoice)
#ordered logit
model3<-polr(psechoice1~grades,data=nels_small,method="logistic")
summary(model3)
#coefficient of grades is negative, that is the probability of attending a 4 year college (best outcome)
#decreases as grades worsen and probability of the worst outcome increases
### Marginal effects ordered logistic and multinomial?

probs <- predict(model1, type = "probs")
head(probs)
coef(model1)
# Extract coefficients
b <- coef(model1)

b2 <- b["2", "grades"]   # 2-year
b3 <- b["3", "grades"]   # 4-year
b1 <- 0                  # base

# Predicted probabilities
p1 <- probs[,1]
p2 <- probs[,2]
p3 <- probs[,3]

# Weighted average beta
beta_bar <- p1*b1 + p2*b2 + p3*b3

# Marginal effects
me1 <- p1*(b1 - beta_bar)
me2 <- p2*(b2 - beta_bar)
me3 <- p3*(b3 - beta_bar)

# Average Marginal Effects
mean(me1)
mean(me2)
mean(me3)


library(mfx)
ologit_model <- model3

ologitmfx(psechoice1 ~ grades,
          data = nels_small)