# Install and load necessary packages
install.packages("ggplot2")
install.packages("TSA")
install.packages("lmtest")
install.packages("urca")
install.packages("xts")

library(ggplot2)
library(TSA)
library(lmtest)
library(urca)
library(xts)

# Set seed for reproducibility
set.seed(123)

# Generate two random walks
n <- 100
x <- cumsum(rnorm(n))
y <- cumsum(rnorm(n))

# Create a data frame
f_data <- data.frame(x, y)

# Plot the time series
ggplot(data, aes(x = 1:n)) +
  geom_line(aes(y = x, color = "x"), size = 1) +
  geom_line(aes(y = y, color = "y"), size = 1) +
  labs(title = "Random Walks: Spurious Regression Example",
       x = "Time", y = "Value") +
  theme_minimal()

# Perform the spurious regression
regression_result <- lm(y ~ x, data = f_data)
summary(regression_result)

# Augmented Dickey-Fuller Test for stationarity
adf_test_x <- ur.df(data$x, type = "drift", lags = 0)
adf_test_y <- ur.df(data$y, type = "drift", lags = 0)

# Print ADF test results
print("ADF Test for x:")
summary(adf_test_x)

print("ADF Test for y:")
summary(adf_test_y)



<<>>=
  # Set seed for reproducibility
  set.seed(123)

# Parameters for AR(1) process
phi <- 0.7  # Autoregressive coefficient
sigma <- 1  # Standard deviation of white noise

# Number of time points
n <- 100

# Simulate AR(1) process
ar1_simulated <- arima.sim(model = list(ar = phi), n = n, sd = sigma)

# Plot the simulated series
plot(ar1_simulated, type = 'l', col = 'blue', lwd = 2, main = 'Simulated AR(1) Process')

adf_test_ar1 <- ur.df(ar1_simulated, type = "drift", lags = 0)
summary(adf_test_ar1)