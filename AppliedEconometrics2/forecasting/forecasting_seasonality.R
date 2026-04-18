#Forecasting housing starts
df <- read.table("fcst6input.dat", header = TRUE)
hs <- ts(df$HSTARTS, start = c(1946, 1), end = c(1994, 11), frequency = 12)
ts_est <- window(hs, end = c(1993, 12))
ts_test <- window(hs, start = c(1994, 1))
plot(hs)
#zoom plt
plot(hs,
     main = "Housing Starts",
     col = "black",
     lwd = 2,
     xlim = c(1990, 1994))

#The figures reveal that there is no trend, so we'll work with the pure seasonal
#model

month <- cycle(ts_est)
month <- factor(month)
t <- 1:length(ts_est)

seasonal <- lm(ts_est ~ month-1)
summary(seasonal)


seasonal_resid<-seasonal$residuals

seasonal_fit<- ts(seasonal$fitted.values,
                 start = start(ts_est),
                 frequency = 12)
par(mfrow=c(2,1))
plot(ts_est, main = "Housing")
lines(seasonal_fit, col = "red")
plot(seasonal_resid,type="l")

t_forecast <- (length(ts_est) + 1):(length(ts_est) + length(ts_test))

month_forecast <- factor(rep(1:12, length.out = length(ts_test)))
newdata <- data.frame(
  t = t_forecast,
  month = factor(rep(1:12, length.out = length(ts_test)))
)

fcst <- predict(seasonal, newdata = newdata)
fcst_ts <- ts(fcst, start = c(1994, 1), frequency = 12)

plot(hs,
     main = "Housing Starts: History and Forecast",
     col = "black",
     lwd = 2,
     xlim = c(1990, 1995))
lines(fcst_ts, col = "red", lwd = 2)

abline(v = 1994, lty = 2, col = "grey")
legend("topleft",
       legend = c("Actual", "Forecast"),
       col = c("black", "red"),
       lty = 1,
       lwd = 2)

