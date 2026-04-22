library(forecast)
library(ggplot2)
#install.packages("patchwork")
library(patchwork)

DataLiquor <- read.csv("C:/Users/fatimasadik/OneDrive - Institute of Business Administration/Econometrics/Spring26_AE2/DataLiquor.csv")
liquor=na.omit(DataLiquor$liquor.sales..1987m1.2014m12)
names(liquor)[1] <- "liquor"
liquor=ts(data=liquor,start=c(1968,1),frequency=12)
plot(liquor,type="l")
#seasonality and trend is quite visible, we also see cylical components
lliquor<-log(liquor)
plot(lliquor,type="l")
lliquor_samp<-window(lliquor, end = c(1993, 12)) #insample
lliquor_out<-window(lliquor, start = c(1994, 1)) #outsample
t <- 1:length(lliquor_samp)
trend<-lm(lliquor_samp~t+I(t^2))
summary(trend)
#both the linear and quadratic terms are highly significant. The adjusted
#R2=90%, reflects the fact that trend is responsible for a large part of the
#variation in liquor sales
trend_resid<-ts(data=trend$residuals,start=c(1968,1),frequency = 12)
fitted_trend <- ts(trend$fitted.values,
                start = start(lliquor_samp),
                frequency = 12)
par(mfrow=c(2,1))
plot(lliquor_samp, main = "Liquor Trend")
lines(fitted_trend, col = "red")
plot(trend_resid)# seasonality is still there

p1 <- ggAcf(trend_resid, lag.max = 20) +
  ggtitle("Residual ACF") +
  theme_minimal()

p2 <- ggPacf(trend_resid, lag.max = 20) +
  ggtitle("Residual PACF") +
  theme_minimal()
lags <- c(5, 10, 15, 20)

lb_table <- data.frame(
  Lag = lags,
  Q_Stat = sapply(lags, function(i)
    Box.test(trend_resid, lag = i, type = "Ljung-Box")$statistic),
  p_value = sapply(lags, function(i)
    Box.test(trend_resid, lag = i, type = "Ljung-Box")$p.value)
)

lb_table
(p1 | p2)

#modeling seasonality and trend together

month <- cycle(lliquor_samp)
month <- factor(month)
sea_trend<-lm(lliquor_samp~t+I(t^2)+month-1)
summary(sea_trend)
#results of regression on quadratic trend and a full
#set of seasonal dummies. The quadratic trend remains highly significant. The
#adjusted R2 rises to 100%,


sea_trend_resid<-ts(data=sea_trend$residuals,start=c(1968,1),frequency = 12)
fitted_sea_trend <- ts(sea_trend$fitted.values,
                   start = start(lliquor_samp),
                   frequency = 12)
par(mfrow=c(2,1))
plot(lliquor_samp, main = "Liquor Trend")
lines(fitted_sea_trend, col = "red")
plot(sea_trend_resid)# seasonality is still there

p1 <- ggAcf(sea_trend_resid, lag.max = 35) +
  ggtitle("Residual ACF") +
  theme_minimal()

p2 <- ggPacf(sea_trend_resid, lag.max = 35) +
  ggtitle("Residual PACF") +
  theme_minimal()
lags <- c(5, 10, 15, 20)

lb_table <- data.frame(
  Lag = lags,
  Q_Stat = sapply(lags, function(i)
    Box.test(sea_trend_resid, lag = i, type = "Ljung-Box")$statistic),
  p_value = sapply(lags, function(i)
    Box.test(sea_trend_resid, lag = i, type = "Ljung-Box")$p.value)
)

lb_table
(p1 | p2)
#The residual sample autocorrelations oscillate and decay slowly
#The Ljung-Box test strongly rejects the white noise null at all displacements.

#the residual sample partial autocorrelations cut off at displacement 3. All of this suggests
#that an AR(3) would provide a good approximation



n <- length(lliquor_samp)

t <- 1:n
t2 <- t^2

# seasonal dummies
season <- model.matrix(~ factor(cycle(lliquor_samp)))[, -1]

xreg <- cbind(t, t2, season)

liquor_model <- arima(
  lliquor_samp,
  order = c(3,0,0),
  xreg = xreg
)
summary(liquor_model)


full_model_resid<-ts(data=liquor_model$residuals,start=c(1968,1),frequency = 12)
full_model_fitted <- ts(lliquor_samp-liquor_model$residuals,
                       start = start(lliquor_samp),
                       frequency = 12)
par(mfrow=c(2,1))
plot(lliquor_samp, main = "Liquor Trend")
lines(full_model_fitted, col = "red")
plot(full_model_resid)

p1 <- ggAcf(full_model_resid, lag.max = 35) +
  ggtitle("Residual ACF") +
  theme_minimal()

p2 <- ggPacf(full_model_resid, lag.max = 35) +
  ggtitle("Residual PACF") +
  theme_minimal()
lags <- c(1,2,3,4,5,6,7,8,9,10)

lb_table <- data.frame(
  Lag = lags,
  Q_Stat = sapply(lags, function(i)
    Box.test(full_model_resid, lag = i, type = "Ljung-Box")$statistic),
  p_value = sapply(lags, function(i)
    Box.test(full_model_resid, lag = i, type = "Ljung-Box")$p.value)
)

lb_table
(p1 | p2)#mostly inside the band
#The Ljung-Box statistics also look good for small and moderate
#displacements, although their p values decrease for longer displacements

#forecast
h <- length(lliquor_out)# forecast horizon
#time index
n_in <- length(lliquor_samp)
future_idx <- (n_in + 1):(n_in + h)

xreg_future <- cbind(future_idx, future_idx^2, 
                     model.matrix(~ factor(cycle(lliquor)))[future_idx, -1])


fcst <- predict(liquor_model, n.ahead = h, newxreg = xreg_future)
# One-liner plot with everything
ts.plot(lliquor_samp, fcst$pred, lliquor_out, 
        col = c("black", "blue", "red"), lwd = 2,
        main = "Forecast vs Actual")
legend("bottomright", c("Historical", "Forecast", "Actual"), 
       col = c("black", "blue", "red"), lwd = 2)


