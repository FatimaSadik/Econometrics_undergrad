//load data
//this file will estimate auto arima for the selected sample of daily data
use daily_data, clear
format date %td
tsset date
gen insample=(date<=mdy(12, 31, 2022))


//ssc install arimaauto
//help arimaauto
//arimaauto z5,sarima(0,1,0,12) if insample==1
keep if insample==1
arimaauto z5, sarima(0,1,0,12)
//the best model is ARMA(2,2)
//we upload the data again and the use arima command to estimate arma(2,2) for the selected sample
use daily_data, clear
format date %td
tsset date
gen insample=(date<=mdy(12, 31, 2022))

arima z5 if insample==1, arima(2,0,2) 
estimates store arima202
//we get one-step ahead forecast for the entire sample
predict chat, y