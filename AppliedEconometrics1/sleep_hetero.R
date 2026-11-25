#install.packages(c("wooldridge", "lmtest", "car", "sandwich"))
library(wooldridge)
library(lmtest)
library(sandwich)
library(stargazer)
data("sleep75")
# Step 1: OLS sleep equation
ols <- lm(sleep ~ totwrk + educ + age + I(age^2) + yngkid + male,
          data = sleep75)
summary(ols)
# Step 2: squared residuals regressed on male
sleep75$uhat2 <- resid(ols)^2
var_model <- lm(uhat2 ~ male, data = sleep75)
summary(var_model)
# Estimated variances
d0 <- coef(var_model)["(Intercept)"]
d1 <- coef(var_model)["male"]
cat("Estimated variance, women:", d0, "\n")
cat("Estimated variance, men: ", d0 + d1, "\n")

# Step 3: is the difference significant?
# (a) t-test on male coefficient: see summary(var_model)
# (b) F-test of H0: delta1 = 0
car::linearHypothesis(var_model, "male = 0")
# (c) Built-in Breusch-Pagan test with male as the only variable
bptest(ols, ~ male, data = sleep75)
bptest(ols, data = sleep75)
bptest(ols, ~ fitted(ols) + I(fitted(ols)^2))
#fail to reject null. not sufficient proof of heteroskedasticity
# Step 1-2: OLS, squared residuals, variance function (as above)
ols <- lm(sleep ~ totwrk + educ + age + I(age^2) + yngkid + male,
          data = sleep75)
sleep75$uhat2 <- resid(ols)^2
var_model <- lm(uhat2 ~ male, data = sleep75)
hhat <- fitted(var_model)
# Safety check: all estimated variances must be positive
stopifnot(all(hhat > 0))
# Step 3: WLS with weights = 1/hhat
wls <- lm(sleep ~ totwrk + educ + age + I(age^2) + yngkid + male,
          data = sleep75, weights = 1 / hhat)
summary(wls)
# Step 4: compare OLS vs FGLS side by side
ols_se <- coeftest(ols, vcov = vcovHC(ols, type = "HC1")) # OLS, robust SEs
wls_se <- coeftest(wls) # FGLS, usual SEs
wls_rob <- coeftest(wls, vcov = vcovHC(wls, type = "HC1")) # FGLS, robust SEs
comparison <- cbind(
  OLS_coef = coef(ols),
  OLS_robust_se = ols_se[, "Std. Error"],
  WLS_coef = coef(wls),
  WLS_se = wls_se[, "Std. Error"],
  WLS_robust_se = wls_rob[, "Std. Error"]
)
round(comparison, 4)
stargazer(ols,wls,type="text")
 