library(stats)
library(tseries)
library(forecast)
UKHP <- read.csv("C:/Users/fatimasadik/OneDrive - Institute of Business Administration/Econometrics/Spring24_AE2/Econometrics_undergrad/UKHP.csv")
plot(UKHP$Average_House_Price,type="l")
acf(UKHP$Average_House_Price,lag.max = 12)
UKHP$LHP<-log(UKHP$Average_House_Price)
DLHP<-diff(UKHP$LHP)
plot(DLHP,type="l")
acf(DLHP,lag.max = 12)
pacf(DLHP,lag.max=12)
#acf dies slowly
#It can be deduced that the frst six autocorrelation coeffcients (then nine through twelve) and the first
#two partial autocorrelation coeffcients (then nine, eleven and twelve) are significant
#mixed arma model seems more appropriate
#arma(1,1)
(arma11<-arima(DLHP, order=c(1,0,1)))
(arma12<-arima(DLHP, order=c(1,0,2)))
(arma32<-arima(DLHP, order=c(3,0,2)))
(arma42<-arima(DLHP, order=c(4,0,2)))
(arma44<-arima(DLHP, order=c(4,0,4)))
#residual diagnosis, h0: error are WN
Box.test(arma42$residuals, type = "Ljung-Box")
#we fail to reject the null hypothesis
(arma42.for <- predict(arma, n.ahead = 3))


