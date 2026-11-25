library(wooldridge); library(lmtest); library(sandwich)
data("smoke")

f <- cigs ~ log(income) + log(cigpric) + educ + age + I(age^2) + restaurn

# 1. OLS and a heteroskedasticity test
ols <- lm(f, data = smoke)
summary(ols)
bptest(ols)                              # H0: homoskedasticity


plot(fitted(ols), resid(ols)^2)

# 2.Exponential variance function: regress log(uhat^2) on the regressors
smoke$logu2 <- log(resid(ols)^2)
aux  <- lm(logu2 ~ log(income) + log(cigpric) + educ + age + I(age^2) + restaurn,
           data = smoke)
hhat <- exp(fitted(aux))                 # always positive

# 4. FGLS (WLS with estimated weights)
fgls <- lm(f, data = smoke, weights = 1 / hhat)
summary(fgls)

# 5. Compare, with robust SEs for both
cbind(OLS_coef    = coef(ols),
      OLS_robust  = coeftest(ols,  vcov = vcovHC(ols,  "HC1"))[, "Std. Error"],
      FGLS_coef   = coef(fgls),
      FGLS_usual  = coeftest(fgls)[, "Std. Error"],
      FGLS_robust = coeftest(fgls, vcov = vcovHC(fgls, "HC1"))[, "Std. Error"])
