
library(wooldridge)
library(ggplot2)

data("wage1")

wage1$gender <- factor(wage1$female,
                       levels = c(0, 1),
                       labels = c("Male", "Female"))

model <- lm(wage ~ female, data = wage1)

ggplot(wage1, aes(x = gender, y = wage)) +
  
  # Distribution shape
  geom_violin(fill = "lightblue",
              alpha = 0.4,
              trim = FALSE,
              width = 0.8) +
  
  # Boxplots
  geom_boxplot(width = 0.15,
               fill = "grey85",
               outlier.shape = NA) +
  
  # Individual observations
  geom_jitter(width = 0.08,
              alpha = 0.35,
              size = 1.3) +
  
  labs(title = "Wage Distribution by Gender",
       x = "Gender",
       y = "Hourly Wage") +
  
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
