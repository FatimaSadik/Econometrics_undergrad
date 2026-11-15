

library(wooldridge)
library(ggplot2)

# Load data
data("wage1")

# Convert gender into labels
wage1$gender <- factor(wage1$female,
                       levels = c(0, 1),
                       labels = c("Male", "Female"))

# Estimate regression
model <- lm(wage ~ female, data = wage1)

# Obtain fitted values for the two groups
pred <- predict(model,
                newdata = data.frame(female = c(0, 1)))

# Plot
ggplot(wage1, aes(x = gender, y = wage)) +
  
  # Boxplots
  geom_boxplot(width = 0.25,
               fill = "grey85",
               outlier.shape = NA) +
  
  # Individual observations
  geom_jitter(width = 0.08,
              alpha = 0.45,
              size = 1.5) +

  
  labs(title = "Wage Distribution by Gender",
       x = "Gender",
       y = "Hourly Wage") +
  
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
