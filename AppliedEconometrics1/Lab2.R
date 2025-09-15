# Install if not available
#install.packages("wooldridge")
# install.packages("dplyr")

library(wooldridge)
library(dplyr)

# Load dataset
data("wage1")

# Simple regression: wage on education
ols_model <- lm(wage ~ educ, data = wage1)
summary(ols_model)

y <- wage1$wage
x <- wage1$educ
y_hat <- fitted(ols_model)
u_hat <- residuals(ols_model)
# verifying algebraic properties of OLS

sum(u_hat)

cov(x, u_hat)

mean_y <- mean(y)
mean_x <- mean(x)

# Predicted value at mean(x)
beta0 <- coef(ols_model)[1]
beta1 <- coef(ols_model)[2]
y_at_meanx <- beta0 + beta1 * mean_x

c(mean_y = mean_y, fitted_at_meanx = y_at_meanx)

cov(y_hat, u_hat)
plot(x, y, pch = 16, col = "steelblue",
     xlab = "Education (years)", ylab = "Wage",
     main = "Wage vs Education with OLS Fit")
abline(ols_model, col = "red", lwd = 2)
points(mean_x, mean_y, col = "darkgreen", pch = 19, cex = 1.5)
text(mean_x, mean_y, labels = "(mean x, mean y)", pos = 4)
