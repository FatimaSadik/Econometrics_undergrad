# -------------------------------
# Lab 1 - Wooldridge Dataset Work
# -------------------------------

# 1. Install packages (Run only once)
install.packages("wooldridge")
install.packages("dplyr")
install.packages("ggplot2")

# 2. Load libraries
library(wooldridge)
library(dplyr)
library(ggplot2)
# 3. Load dataframe from the wooldridge package
data("fertil1")

# 4. View summary statistics for all columns
summary(fertil1)

summary(fertil1$age)

# 5. Create a new variable 'age2' (age squared)
fertil1$age2 <- fertil1$age^2

# 6. Filter data for year == 1972
fertil1_1972 <- fertil1 %>%
  filter(year == 72)

# 8. Check the filtered data
summary(fertil1_1972)
head(fertil1_1972)
unique(fertil1_1972$year)
names(fertil1_1972)
# Histogram of age

ggplot(fertil1_1972, aes(x = age)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Age (1972)", x = "Age", y = "Count")

ggplot(fertil1_1972, aes(y = educ)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(
    title = "Boxplot of Education Years (1972)",
    y = "Years of Education"
  )


  