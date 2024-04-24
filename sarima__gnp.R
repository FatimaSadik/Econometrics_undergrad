library(astsa)
gnpgr = diff(log(gnp))  # growth rate of GNP
main <- "Quarterly growth rate\n of U.S. GNP"
plot(gnpgr, main = main, ylab = "Growth rate")
# There is a different mean in each quarter, but forego the seasonal effect
# This is obvious in the following plot, which plots (in order), separating
# by quarter monthplot(gnpgr, main = main, ylab = 'Growth rate', xlab =
# 'Quarter')
par(mfrow = c(1, 2))
TSA::acf(gnpgr, 24, main = main)
pacf(gnpgr, 24, main = main)
mod1 <- sarima(gnpgr, p = 1, d = 0, q = 0, details = FALSE)  # AR(1)
print(mod1$ttable)
mod2 <- sarima(gnpgr, p = 0, d = 0, q = 2, details = FALSE)  # MA(2)
print(mod2$ttable)
mod3<-sarima(gnpgr, p = 1, d = 0, q = 2, details = FALSE)
print(mod3$ttable)