passengers <- read_excel("C:/Users/fatimasadik/OneDrive - Institute of Business Administration/Econometrics/Spring24_AE2/Econometrics_undergrad/passangers.xlsx", col_types = c("numeric"))

#tasks: 
#1) plot the series and make observations about stationarity, 
plot(passengers$`Number of Passengers`,type="l")
#2) transform the series and then revise your conclusion, which transformation would you suggest?
lpass<-log(passengers$`Number of Passengers`)
## Take first differences of transformed series to remove trend 
fd<-diff(lpass)
## and plot the transformed and differenced data.
#3)plot acf and pacf and then suggest ARMA (p,q) order of the model
#4)Take seasonal differences of the transformed and differenced series.
sfd = diff(fd, lag=12)
## Plot final time series.
plot(sfd,type="l")
#5)plot acf and pacf of the final time series
acf(sfd)
pacf(sfd)
#6)fit MA(1) to the transformed series
(ma1<-arima(sfd,order=c(0,0,1)))

(ma2<-arima(sfd,order=c(0,0,2)))
#7)Use the Box-Ljung test to determine if the residuals are 
## random up to 30 lags
BT = Box.test(ma2$residuals, lag=30, type = "Ljung-Box", fitdf=2)
print(BT)
