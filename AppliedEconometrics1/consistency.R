# Consistency of the sample mean and the OLS slope
# Run in RStudio: select all (Ctrl+A) and click Run.

set.seed(123)
sizes <- c(5, 10, 100, 1000, 10000)   # sample sizes

# ---------- 1. Sample mean ----------
# Data: Normal with true mean 2
for (n in sizes) {
  x <- rnorm(n, mean = 2, sd = 3)
  cat("n =", n, " sample mean =", round(mean(x), 3), "\n")
}
# The sample mean gets closer to 2 as n grows.

# ---------- 2. OLS slope ----------
# Model: y = 1 + 2*x + u, true slope = 2
for (n in sizes) {
  x <- rnorm(n, mean = 5, sd = 2)
  u <- rnorm(n, mean = 0, sd = 1)
  y <- 1 + 2 * x + u
  b1 <- coef(lm(y ~ x))[2]
  cat("n =", n, " beta1-hat =", round(b1, 3), "\n")
}
# beta1-hat gets closer to 2 as n grows.

# ---------- 3. Histograms: what happens as n grows ----------
# For each n, draw 500 samples and compute the estimate each time.
# Then plot a histogram of the 500 estimates.

par(mfrow = c(2, 4))   # 2 rows (mean, slope) x 4 columns (n)

# Row 1: sample mean
for (n in sizes) {
  est <- numeric(500)
  for (r in 1:500) {
    est[r] <- mean(rnorm(n, mean = 2, sd = 3))
  }
  hist(est, breaks = seq(-50, 50, by = 0.1), xlim = c(-1, 5),
       main = paste("Sample mean, n =", n), xlab = "estimate")
  abline(v = 2, col = "red", lwd = 2)
}

# Row 2: OLS slope
for (n in sizes) {
  est <- numeric(500)
  for (r in 1:500) {
    x <- rnorm(n, mean = 5, sd = 2)
    y <- 1 + 2 * x + rnorm(n)
    est[r] <- coef(lm(y ~ x))[2]
  }
  hist(est, breaks = seq(-50, 50, by = 0.02), xlim = c(1.5, 2.5),
       main = paste("OLS slope, n =", n), xlab = "estimate")
  abline(v = 2, col = "red", lwd = 2)
}
