set.seed(123)
library(stats)
# Simulate MA(1) process
n <- 100  # Number of observations
phi <- -0.5  # MA parameter
epsilon <- rnorm(n)  # White noise
y <- numeric(n)
for (i in 2:n) {
  y[i] <- phi * epsilon[i - 1] + epsilon[i]
}
# Adjust plot device size
options(repr.plot.width=10, repr.plot.height=10) # Adjust width and height as needed
plot(y, type = 'l', main = "Simulated MA(1) Process", ylab = "Value")

acf(y,lag.max = 10)
pacf(y,lag.max = 10)

#the MA(1) has an acf that is significant for only lag 1,
#while the pacf declines geometrically, and is significant until lag 7. The acf
#at lag 1 and all of the pacfs are negative as a result of the negative
#coefficient in the MA generating process.
#since the second coefficient on the lagged error term in the MA is
#negative, the acf and pacf alternate between positive and negative.

#simulate AR(1)
x <- numeric(n)
x[1]<-0
for (t in 2:n) {
  x[t] <- -0.9 * x[t - 1] + epsilon[t]
}
acf(x)
pacf(x)
#For the autoregressive model of order 1 with a fairly high coefficient –
#i.e., relatively close to 1 – the autocorrelation function would be expected
#to die away relatively slowly

options(repr.plot.width=10, repr.plot.height=10) # Adjust width and height as needed
plot(x, type = 'l', main = "Simulated AR(1) Process", ylab = "Value")


#simulate AR(2)
x2 <- numeric(n)

x2[1]<-0
x2[2] <- 0
for (i in 3:n) {
  x2[i] <- 0.5 * x2[i - 1] + 0.2*x2[i-2]+epsilon[i]
}


options(repr.plot.width=10, repr.plot.height=10) # Adjust width and height as needed
plot(x2, type = 'l', main = "Simulated AR(2) Process", ylab = "Value")
