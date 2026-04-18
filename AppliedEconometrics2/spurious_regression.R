# Install and load necessary packages
install.packages("ggplot2")
#install.packages("TSA")
install.packages("lmtest")
install.packages("urca")
install.packages("xts")

library(ggplot2)
#library(TSA)
library(lmtest)
library(urca)
library(xts)

# Set seed for reproducibility
set.seed(123)

# Generate two random walks
n <- 100
x <- cumsum(rnorm(n))
y <- cumsum(rnorm(n))
y2<-0.1+0.2*x+rnorm(n)
# Create a data frame
data <- data.frame(x, y,y2)

# Plot the time series
ggplot(data, aes(x = 1:n)) +
  geom_line(aes(y = x, color = "x"), size = 1) +
  geom_line(aes(y = y, color = "y"), size = 1) +
  labs(title = "Random Walks: Spurious Regression Example",
       x = "Time", y = "Value") +
  theme_minimal()

# Perform the spurious regression
regression_result <- lm(y ~ x, data = data)
summary(regression_result)

ols<-lm(y2~x,data=data)
summary(ols)

data("AirPassengers")
plot(AirPassengers)

