
//load data
use daily_data, clear
format date %td
tsset date
gen insample=(date<=mdy(12, 31, 2022))


//ssc install arimaauto
//help arimaauto
//arimaauto z5,sarima(0,1,0,12) if insample==1
keep if insample==1
arimaauto z5, sarima(0,1,0,12)
//arimaauto z5 if insample==1, sarima(0,1,0,12) iterate(1000)
estimates store myautoarima


arima z5 if insample==1, arima(2,0,2) 
estimates store arima202
predict zhat, y
predict zhatdy, dynamic(mdy(12, 31, 2022)) y

