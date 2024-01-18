install.packages("wooldridge")
install.packages("car")
install.packages("dplyr")
library("wooldridge")
library(car)
library(dplyr)

data("phillips")
plot(phillips$unem,type = "l", col = "blue", lwd = 2,
     main = "Unemployment",
     xlab = "Time", ylab = "Unemployment Rate")

plot(phillips$inf,type = "l", col = "blue", lwd = 2,
     main = "Inflation",
     xlab = "Time", ylab = "Inflation Rate")

  plot(phillips$unem, type = "l", col = "blue", lwd = 2,
       main = "Inflation and Unemployment",
       xlab = "Time", ylab = "Unemployment Rate")

# Add inflation to the same plot
lines(phillips$inf, col = "red", lwd = 2)

# Add a legend
#legend("topright", legend = c("Unemployment", "Inflation"), col = c("blue", "red"), lwd = 2)

<<>>=
  #run a static simple time series regression
  phillips_static<-lm(inf~unem, data=phillips)
summary(phillips_static)
#testing for serial correlation
uhat<-phillips_static$residuals
L.uhat<-lag(uhat,n=1)
serial_corr<-lm(uhat~L.uhat)
summary(serial_corr)

#Durbin Watson Test
dw_test <- durbinWatsonTest(phillips_static)
dw_statistic <- sum(diff(uhat)^2) / sum(uhat^2)
print(dw_statistic)
print(dw_test)

#expectation augmented phillips curve
dinf<-diff(phillips$inf)
expectation_pc<-lm(dinf~phillips$unem[2:56])
summary(expectation_pc)
dw_test2 <- durbinWatsonTest(expectation_pc)
print(dw_test2)
##test for serial correlation
#adaptive expectation
dunemp<-diff(phillips$unem)
adaptive_pc<-lm(dinf~dunemp)
summary(adaptive_pc)

plot(phillips$cunem, type = "l", col = "blue", lwd = 2,
     main = "Inflation and Unemployment first Diff",
     xlab = "Time", ylab = "Unemployment Rate")

# Add inflation to the same plot
lines(phillips$cinf, col = "red", lwd = 2)

# Add a legend
legend("topright", legend = c("Unemployment", "Inflation"), col = c("blue", "red"), lwd = 2)