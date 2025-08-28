# Install and load the quantmod package
install.packages("quantmod")
library(quantmod)

# Define the symbol for GDP data from FRED (Federal Reserve Economic Data)
gdp_symbol <- "GDPC1"

# Download GDP data
getSymbols(gdp_symbol, src = "FRED")

# Plot the GDP data
plot(GDPC1, main = "US GDP Data", ylab = "GDP (in billions of dollars)", col = "blue")
lnGDPC1<-log(GDPC1)
plot(lnGDPC1, main = "US GDP Data", ylab = "Log(GDP)", col = "blue")
dlogGDP<-diff(lnGDPC1)
plot(dlogGDP, main = "US GDP Data", ylab = "GDP Growth", col = "blue")