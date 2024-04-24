passengers <- read_excel("C:/Users/fatimasadik/OneDrive - Institute of Business Administration/Econometrics/Spring24_AE2/Econometrics_undergrad/passangers.xlsx", col_types = c("numeric"))
#tasks: 
#1) plot the series and make observations about stationarity, 
#2) transform the series and then revise your conclusion, which transformation would you suggest?
## Take first differences of transformed series to remove trend 
## and plot the transformed and differenced data.
#3)plot acf and pacf and then suggest ARMA (p,q) order of the model
#4)Take seasonal differences of the transformed and differenced series.
#sfd = diff(fd, lag=12)
## Plot final time series.
#5)plot acf and pacf of the final time series
#6)fit MA(1) to the transformed series
#7)Use the Box-Ljung test to determine if the residuals are 
## random up to 30 lags
#BT = Box.test(ma$residuals, lag=30, type = "Ljung-Box", fitdf=2)
