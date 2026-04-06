
install.packages("wooldridge")
install.packages("car")
install.packages("dplyr")
library("wooldridge")
library(car)
library(dplyr)

data("phillips")
phillips_static <- lm(inf ~ unem, data = phillips)
summary(phillips_static)

uhat <- phillips_static$residuals
plot(uhat)
L.uhat <- lag(uhat, n = 1)
cor(uhat,L.uhat,use="complete.obs")

#Durbin Watson Test
dw_test <- durbinWatsonTest(phillips_static)
dw_statistic <- sum(diff(uhat)^2) / sum(uhat^2)
print(dw_statistic)
print(dw_test)

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

# run transformed regression gls (FGL)
cochrane_orcutt <- lm(inf_star ~ unem_star, data = phillips_co)
summary(cochrane_orcutt)

uhat_co <- cochrane_orcutt$residuals
plot(uhat_co)
dw_test2 <- durbinWatsonTest(cochrane_orcutt)
print(dw_test2)
wn<-rnorm(100,mean=0,sd=1)
plot(wn)
