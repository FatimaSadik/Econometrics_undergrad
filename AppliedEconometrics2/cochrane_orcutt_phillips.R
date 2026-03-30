
install.packages("wooldridge")
install.packages("car")
install.packages("dplyr")
library("wooldridge")
library(car)
library(dplyr)

data("phillips")
phillips_static <- lm(inf ~ unem, data = phillips)

uhat <- phillips_static$residuals
L.uhat <- lag(uhat, n = 1)

serial_corr <- lm(uhat ~ L.uhat - 1)
rho <- coef(serial_corr)[1]   # estimated rho
rho

# create lagged variables
phillips$inf_lag  <- lag(phillips$inf, 1)
phillips$unem_lag <- lag(phillips$unem, 1)

# transform variables
phillips$inf_star  <- phillips$inf  - rho * phillips$inf_lag
phillips$unem_star <- phillips$unem - rho * phillips$unem_lag

# drop NA (first observation lost)
phillips_co <- na.omit(phillips)

# run transformed regression
cochrane_orcutt <- lm(inf_star ~ unem_star, data = phillips_co)
summary(cochrane_orcutt)