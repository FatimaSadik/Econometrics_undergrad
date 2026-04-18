rm(list=ls())
setwd("C:/Users/fatimasadik/OneDrive - Institute of Business Administration/Econometrics/Spring26_AE2/R_Employment")

library(car)
library(tseries)
library(forecast)
library(ggplot2)
data1 = read.csv("DataEmployment1.csv",header=TRUE)
caemp = ts(data1[,"CAEMP"],start=c(1961,1),frequency=4)
caempsamp = ts(caemp[5:132],start=c(1962,1),frequency=4)
ts.plot(caemp, main="Canadian Employment Index",ylab = "Canadian Employment Index", xlim=c(1962,1993.75),ylim=c(80,115))
#No trend or seasonality, but we see cyclical behaviour
acf(caemp) 
pacf(caemp)
#sample autocorrelations damp slowly
#sample partial autocorrelations cut off
#Likely AR process
#plot(acf(caempsamp,plot=F),xlim=c(0,3),ylim=c(-1,1),main="Autocorrelation")
#plot(acf(caempsamp,plot=F,type="p"),xlim=c(0,3),ylim=c(-1,1), main="Partial Autocorrelation")


caemp.ma4 <- arima(caemp, order=c(0,0,4))
fitted.ma4 <-caemp -caemp.ma4$residuals
resid.ma4 <- caemp.ma4$residuals
S1<-summary(caemp.ma4)
# Plot actual + fitted
par(mfrow=c(2,1))

plot(caemp, type="l", col="red", lwd=2,
     main="Actual vs Fitted", ylab="Employment")
lines(fitted.ma4, col="green", lwd=2)

plot(resid.ma4, type="l", col="blue", lwd=1,
     main="Residuals", ylab="Residuals")
abline(h=0)
# Legend
legend("topleft",
       legend=c("Residual", "Actual", "Fitted"),
       col=c("blue","red","green"),
       lty=1, lwd=2, cex=0.8)
mtext("MA(4)", side=3, line=-2, outer=TRUE, cex=1.3)
# are residual WN? No
Box.test(resid.ma4, lag=12, type="Ljung-Box")

ggAcf(resid.ma4, lag.max = 20) +
  ggtitle("Residual ACF") +
  xlab("Lag") +
  ylab("ACF") +
  theme_minimal()

lags <- c(5, 10, 15, 20)

data.frame(
  Lag = lags,
  Q_Stat = sapply(lags, function(i)
    Box.test(resid.ma4, lag = i, type = "Ljung-Box")$statistic
  ),
  p_value = sapply(lags, function(i)
    Box.test(resid.ma4, lag = i, type = "Ljung-Box")$p.value
  )
)

checkresiduals(caemp.ma4)
# AR(2)


caemp.ar.model <- arma(caemp, order=c(2,0))
fitted.ar <- caemp.ar.model$fitted.values
resid.ar <- caemp.ar.model$residuals

S2<-summary(caemp.ar.model)
par(mfrow=c(2,1))

plot(caemp, type="l", col="red", lwd=2,
     main="Actual vs Fitted", ylab="Employment")
lines(fitted.ar, col="green", lwd=2)

plot(resid.ar, type="l", col="blue", lwd=1,
     main="Residuals", ylab="Residuals")
abline(h=0)
# Legend
legend("topleft",
       legend=c("Residual", "Actual", "Fitted"),
       col=c("blue","red","green"),
       lty=1, lwd=2, cex=0.8)
mtext("AR(2)", side=3, line=-2, outer=TRUE, cex=1.3)
# are residual WN? yes
Box.test(resid.ar, lag=2, type="Ljung-Box")

ggAcf(resid.ar, lag.max = 20) +
  ggtitle("Residual ACF") +
  xlab("Lag") +
  ylab("ACF") +
  theme_minimal()

lags <- c(2,5, 10, 15, 20)

data.frame(
  Lag = lags,
  Q_Stat = sapply(lags, function(i)
    Box.test(resid.ar, lag = i, type = "Ljung-Box")$statistic
  ),
  p_value = sapply(lags, function(i)
    Box.test(resid.ar, lag = i, type = "Ljung-Box")$p.value
  )
)

checkresiduals(caemp.ar.model)
#ARMA(2,1)
caemp.arma<- arma(caemp, order=c(2,1))
fitted.arma <- caemp.arma$fitted.values
resid.arma <- caemp.arma$residuals

S3<-summary(caemp.arma)
par(mfrow=c(2,1))

plot(caemp, type="l", col="red", lwd=2,
     main="Actual vs Fitted", ylab="Employment")
lines(fitted.arma, col="green", lwd=2)

plot(resid.arma, type="l", col="blue", lwd=1,
     main="Residuals", ylab="Residuals")
abline(h=0)
# Legend
legend("topleft",
       legend=c("Residual", "Actual", "Fitted"),
       col=c("blue","red","green"),
       lty=1, lwd=2, cex=0.8)
mtext("ARMA(2,1)", side=3, line=-1, outer=TRUE, cex=1.3)
# are residual WN? 
Box.test(resid.arma, lag=2, type="Ljung-Box")

ggAcf(resid.ar, lag.max = 20) +
  ggtitle("Residual ACF") +
  xlab("Lag") +
  ylab("ACF") +
  theme_minimal()

lags <- c(2,5, 10, 15, 20)

data.frame(
  Lag = lags,
  Q_Stat = sapply(lags, function(i)
    Box.test(resid.arma, lag = i, type = "Ljung-Box")$statistic
  ),
  p_value = sapply(lags, function(i)
    Box.test(resid.arma, lag = i, type = "Ljung-Box")$p.value
  )
)

checkresiduals(caemp.ar.model)
# Which model is better? MA(4), AR(2), or ARMA(2,1). The one with the lower AIC=-2Log(L)+2k
S1$aic
S2$aic
S3$aic

#forecast

h <- 4
caemp.ma4 <- arima(caempsamp, order=c(0,0,4))
fcst_ma4 <- forecast(caemp.ma4, h = h, level = 95)

autoplot(fcst_ma4) +
  autolayer(caemp, series = "Actual")+
  ggtitle("MA(4) Forecast with 95% Confidence Interval") +
  xlab("Time") + ylab("Employment") +
  theme_minimal()
#The 4-quarter-ahead extrapolation forecast reverts very quickly to the mean of the employment index
#The quick reversion of the MA(4) forecast to the mean is a manifestation of the short memory
#of moving average processes. Recall, in particular, that an MA(4) process has
#a four-period memory—all autocorrelations are 0 beyond displacement 4
#Thus, all forecasts more than four steps ahead are simply equal to the unconditional
#mean (100.2),

#AR(2)
caemp.ar.model <- Arima(caempsamp, order=c(2,0,0))
fcst_ar2 <- forecast(caemp.ar.model, h = h,level = 95)

autoplot(fcst_ar2) +
  autolayer(caemp, series = "Actual") +
  ggtitle("AR(2) Forecast with Confidence Intervals") +
  xlab("Time") + ylab("Employment") +
  theme_minimal()
#(ARMA(2,1))

caemp.arma<- Arima(caempsamp, order=c(2,0,1))
fcst_arma21 <- forecast(caemp.arma, h = h,level=95)

autoplot(fcst_arma21) +
  autolayer(caemp, series = "Actual") +
  ggtitle("ARMA(2,1) Forecast with Confidence Intervals") +
  xlab("Time") + ylab("Employment") +
  theme_minimal()



actual <- tail(caemp, h)

# Forecast errors
err_ma4  <- actual - fcst_ma4$mean
err_ar2  <- actual - fcst_ar2$mean
err_arma21 <- actual - fcst_arma21$mean

# MSE
mse_ma4  <- mean(err_ma4^2)
mse_ar2  <- mean(err_ar2^2)
mse_arma21 <- mean(err_arma21^2)

rmse_ma4  <- sqrt(mse_ma4)
rmse_ar2  <- sqrt(mse_ar2)
rmse_arm21 <- sqrt(mse_arma21)