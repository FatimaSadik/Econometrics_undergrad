############################################################
# HETEROSKEDASTICITY: OLS → BP Test → White Test → WLS
############################################################

library(wooldridge)
data(wage2)

############################################################
# 1. OLS REGRESSION
############################################################

ols <- lm(wage ~ educ, data = wage2)
summary(ols)

uhat <- ols$residuals

# Residual plot
plot(uhat, type = "l", main = "OLS Residuals", ylab = "u_hat")


############################################################
# 2. BREUSCH–PAGAN TEST (MANUAL)
############################################################

# Squared residuals
uhat2 <- uhat^2

# Aux regression: u_hat^2 on educ
reg_bp <- lm(uhat2 ~ wage2$educ)
summary(reg_bp)

# LM statistic = n * R^2
n  <- nrow(wage2)
R2 <- summary(reg_bp)$r.squared
LM <- n * R2
pval <- 1 - pchisq(LM, df = 1)  # 1 regressor: educ

cat("\nBreusch-Pagan Test:\n")
cat("LM =", LM, "  p-value =", pval, "\n")


############################################################
# 3. WHITE TEST (MANUAL)
############################################################

# Generate yhat and yhat^2
yhat  <- ols$fitted.values
yhat2 <- yhat^2

# Aux regression for White test
reg_white <- lm(uhat2 ~ yhat + yhat2)
summary(reg_white)

R2_white <- summary(reg_white)$r.squared
LM_white <- n * R2_white
pval_white <- 1 - pchisq(LM_white, df = 2)  # regressors: yhat, yhat^2

cat("\nWhite Test:\n")
cat("LM =", LM_white, "  p-value =", pval_white, "\n")


############################################################
# 4. WEIGHTED LEAST SQUARES (WLS)
############################################################

# Predicted variance from BP regression
sigma2hat <- reg_bp$fitted.values

# Replace any negative/zero values
sigma2hat[sigma2hat <= 0] <- min(sigma2hat[sigma2hat > 0])

# Define weights
w <- 1 / sigma2hat

# Weighted regression
wls <- lm(wage ~ educ, data = wage2, weights = w)
summary(wls)
summary(ols)

############################################################
# 5. CHECK IF WLS REMOVED HETEROSKEDASTICITY
############################################################

# Squared WLS residuals
wls_uhat2 <- (wls$residuals)^2

# Fitted values and square
wls_yhat  <- wls$fitted.values
wls_yhat2 <- wls_yhat^2

# White-style test on WLS residuals
reg_wls_white <- lm(wls_uhat2 ~ wls_yhat + wls_yhat2)
summary(reg_wls_white)

cat("\nWhite-style Test on WLS Residuals:\n")
R2_wls   <- summary(reg_wls_white)$r.squared
LM_wls   <- n * R2_wls
pval_wls <- 1 - pchisq(LM_wls, df = 2)

cat("LM =", LM_wls, "  p-value =", pval_wls, "\n")

#ROBUST SE
ols2<-lm(wage~educ,data=wage2)
summary(ols2)
library(lmtest)
library(sandwich)
vcov = vcovHC(ols2, type = "HC1")
coeftest(ols2,vcov =vcov)

# Usual OLS standard errors
ols_summary <- summary(ols2)
ols_se <- coef(ols_summary)[, "Std. Error"]

# Robust standard errors
robust_se <- sqrt(diag(vcovHC(ols2, type = "HC1")))

# Combine into a table
se_table <- data.frame(
  Coefficient = coef(ols2),
  OLS_SE = ols_se,
  Robust_SE = robust_se
)

se_table


#Ramsey reset test

yhat<-ols$fitted.values
yhat2<-yhat^2
yhat3<-yhat^3
model1<-lm(wage2$wage~wage2$educ+yhat2+yhat3) # unrestrcited model


t
resettest(wage~educ, data=wage2)
