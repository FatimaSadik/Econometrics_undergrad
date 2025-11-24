set.seed(123)  
# Sets the random seed so results are the same every time you run the code.

# Population: skewed exponential distribution
population <- rexp(100000, rate = 1)  
# Creates a large population drawn from an exponential distribution (highly skewed).
# This helps show how the CLT works even when the population is not normal.

# Function to compute many sample means for a given sample size n
get_means <- function(n) {
  replicate(5000, mean(sample(population, n, replace = TRUE)))
  # replicate(): repeats the sampling procedure 5000 times
  # sample(): draws a sample of size n from the population
  # mean(): computes the sample mean
}

# Sample sizes to illustrate the CLT
means_5   <- get_means(5)     # Many sample means for n = 5
means_30  <- get_means(30)    # Many sample means for n = 30
means_100 <- get_means(100)   # Many sample means for n = 100

# Plot the histograms
par(mfrow = c(3,1))  
# Makes a 3-row plotting layout so histograms appear one above the other.

hist(means_5,  main = "Sampling Distribution (n = 5)", 
     xlab = "Sample Mean")
# Histogram shows sampling distribution for n = 5 — still skewed.

hist(means_30, main = "Sampling Distribution (n = 30)", 
     xlab = "Sample Mean")
# For n = 30, the distribution looks more bell-shaped.

hist(means_100, main = "Sampling Distribution (n = 100)", 
     xlab = "Sample Mean")
# For n = 100, the distribution is very close to normal — the CLT in action!
