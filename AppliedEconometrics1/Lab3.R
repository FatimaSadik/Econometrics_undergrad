set.seed(123)

# Parameters
beta0 <- 1
beta1 <- 2
n <- 50       # sample size
R <- 1000     # number of replications

# Storage for slope estimates
b1_hat <- numeric(R)

for (r in 1:R) {
  x <- rnorm(n, mean = 0, sd = 1)
  u <- rnorm(n, mean = 0, sd = 1)
  y <- beta0 + beta1 * x + u
  fit <- lm(y ~ x)
  b1_hat[r] <- coef(fit)[2]
}

# Mean of estimates
mean(b1_hat)

hist(b1_hat, breaks = 30, col = "lightblue", main = "Sampling Distribution of Beta1 Estimates",
     xlab = expression(hat(beta)[1]))
abline(v = beta1, col = "red", lwd = 2, lty = 2)