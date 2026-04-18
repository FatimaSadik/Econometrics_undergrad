#Forecasting Retail Sales
df <- read.table("fcst5input.dat", header = TRUE)

df_sub <- subset(df,RTRR!="NA")

retail_sales <- ts(df_sub$RTRR, start = c(1955, 1), end = c(1994, 12), frequency = 12)
plot(retail_sales)# visible non-linear trend

ts_est <- window(retail_sales, end = c(1993, 12))
ts_test <- window(retail_sales, start = c(1994, 1))
t <- 1:length(ts_est)
linear_trend <- lm(ts_est ~ t)
summary(linear_trend)
#The trend appears highly significant  and the regression's R2 is high.
install.packages("lmtest")
library(lmtest)
dwtest(linear_trend)
#reject the null hypothesis of no autocorrelation, positive serial correlation
#residual plot and fitted plot
par(mfrow=c(2,1))
fitted_ts <- ts(linear_trend$fitted.values,
                start = start(ts_est),
                frequency = 12)
plot(ts_est, main = "RTRR with Linear Trend")
lines(fitted_ts, col = "red")
plot(linear_trend$residuals)
#residuals follow a quadratic pattern. This indicates that linear trend model does not
#capture the true trend

quad_trend <- lm(ts_est ~ t+I(t^2))
summary(quad_trend)# R2 is almost 1
dwtest(quad_trend) # still positive auto correlation, but could be due to cyclical components

fitted_ts2 <- ts(quad_trend$fitted.values,
                start = start(ts_est),
                frequency = 12)
par(mfrow=c(2,1))
plot(ts_est, main = "RTRR with Linear Trend")
lines(fitted_ts2, col = "red")
plot(quad_trend$residuals,type="l")

#log-linear trend
loglin_trend <- lm(log(ts_est) ~ t)
summary(loglin_trend)
#cant compare linear trend, quadratic trend model with log-linear model.

exp_trend <- nls(ts_est ~ a * exp(b * t),
                 start = list(a = ts_est[1], b = 0.001))

summary(exp_trend)
fitted_vals <- predict(exp_trend)
SSE <- sum((ts_est - fitted_vals)^2)
SST <- sum((ts_est - mean(ts_est))^2)

R2_exp <- 1 - SSE/SST
R2_exp




fitted_ts3 <- ts(fitted_vals,
                 start = start(ts_est),
                 frequency = 12)
par(mfrow=c(2,1))
plot(ts_est, main = "RTRR with exponential Trend")
lines(fitted_ts3, col = "red")
resid_vals<-ts_est-fitted_ts3
plot(resid_vals,type="l")


results <- c(
  R2_exp = R2_exp,
  R2_linear = summary(linear_trend)$r.squared,
  R2_quad = summary(quad_trend)$r.squared
)

print(results)

AIC_linear <- AIC(linear_trend)
BIC_linear <- BIC(linear_trend)
AIC_quad <- AIC(quad_trend)
BIC_quad <- BIC(quad_trend)
AIC_exp <- AIC(exp_trend)
BIC_exp <- BIC(exp_trend)

model_comparison <- data.frame(
  Model = c("Linear", "Quadratic","Exponential"),
  AIC = c(AIC_linear, AIC_quad, AIC_exp),
  BIC = c(BIC_linear, BIC_quad, BIC_exp)
)

print(model_comparison)


#We choose quadratic trend
#future time index
t_forecast <- (length(ts_est) + 1):(length(ts_est) + length(ts_test))
newdata <- data.frame(
  t = t_forecast
)
fcst_quad <- predict(quad_trend, newdata = newdata)
a <- coef(exp_trend)["a"]
b <- coef(exp_trend)["b"]
fcst_exp <- a * exp(b * t_forecast)
fcst_ts_quad <- ts(fcst_quad, start = c(1994, 1), frequency = 12)
fcst_ts_exp <- ts(fcst_exp, start = c(1994, 1), frequency = 12)
# Combine all series to get correct y-axis limits
ylim_range <- range(ts_test, fcst_ts_quad, fcst_ts_exp, na.rm = TRUE)
# Plot actual data
plot(ts_test,
     main = "Retail Sales Forecast: Quadratic vs Exponential Trend",
     ylim = ylim_range,
     col = "black",
     lwd = 2)

# Add forecasts
lines(fcst_ts_quad, col = "red", lwd = 2)
lines(fcst_ts_exp, col = "green", lwd = 2)

# Add legend
legend("topleft",
       legend = c("Actual", "Quadratic Trend", "Exponential Trend"),
       col = c("black", "red", "green"),
       lty = 1,
       lwd = 2)


plot(retail_sales,
     main = "Retail Sales: History and Forecast",
     col = "black",
     lwd = 2,
     xlim = c(1990, 1994))
lines(fcst_ts_quad, col = "red", lwd = 2)
lines(fcst_ts_exp, col = "green", lwd = 2)
abline(v = 1994, lty = 2, col = "grey")
legend("topleft",
       legend = c("Actual", "Quadratic Forecast", "Exponential Forecast"),
       col = c("black", "red", "green"),
       lty = 1,
       lwd = 2)
RMSE_quad <- sqrt(mean((ts_test - fcst_ts_quad)^2))
RMSE_exp  <- sqrt(mean((ts_test - fcst_ts_exp)^2))

c(RMSE_quad = RMSE_quad,
  RMSE_exp = RMSE_exp)



