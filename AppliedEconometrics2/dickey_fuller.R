install.packages("tseries")
install.packages("urca")

library(tseries)
library(urca)

set.seed(123)
n <- 100
y_stationary <- arima.sim(model = list(ar =0.6), n = n)

plot(y_stationary, main = "Stationary Series")
y_nonstationary <- cumsum(rnorm(n))

plot(y_nonstationary, main = "Non-Stationary Series (Random Walk)",type="l")

adf.test(y_stationary)
# p<0.05, reject null hypothesis of non-stationary: (stationary)
adf.test(y_nonstationary)
# p>0.05, fail to reject null of non-stationary: (non-stationary)

df_test1 <- ur.df(y_nonstationary, type = "drift", lags = 1)
summary(df_test1)
# |tau|<|c.v| :0.3721<3.5: fail to reject null: non-stationary

df_test2 <- ur.df(y_nonstationary, type = "trend", lags = 1)
summary(df_test2)

diff_y <- diff(y_nonstationary)

plot(diff_y, main = "Differenced Series",type="l")

adf.test(diff_y)
##

