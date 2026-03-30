# Install packages
install.packages(c("nnet", "MASS", "PoEdata"))

# Load packages
library(nnet)
library(MASS)
library(PoEdata)

# Load data
data("nels_small")
head(nels_small)

# Multinomial logit
model_mnl <- multinom(psechoice ~ grades, data = nels_small)
summary(model_mnl)

# Predicted probabilities for each observation
probs <- predict(model_mnl, type = "probs")
head(probs)

# Extract coefficients
b <- coef(model_mnl)
b1 <- 0         # base category
b2 <- b["2", "grades"]
b3 <- b["3", "grades"]

# Probabilities
p1 <- probs[,1]
p2 <- probs[,2]
p3 <- probs[,3]

# Compute weighted beta
beta_bar <- p1*b1 + p2*b2 + p3*b3

# Marginal effects
me1 <- p1 * (b1 - beta_bar)
me2 <- p2 * (b2 - beta_bar)
me3 <- p3 * (b3 - beta_bar)

# Average marginal effects
AME <- data.frame(NoCollege = mean(me1),
                  TwoYear   = mean(me2),
                  FourYear  = mean(me3))
AME



#Find out Marginal Effect at representative value (grade=7) for each college choice