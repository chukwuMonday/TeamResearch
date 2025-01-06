# Load necessary libraries
library(ggplot2)

# Read the dataset
data <- read.csv("C:/Users/hency/Documents/TeamResearch/russian_demography.csv")

# Boxplot: birth_rate vs. region
ggplot(data, aes(x = region, y = birth_rate)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16) +
  labs(title = "Boxplot of Birth Rate by Region", x = "Region", y = "Birth Rate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Histogram with normal curve overlay
ggplot(data, aes(x = birth_rate)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(data$birth_rate, na.rm = TRUE), 
                            sd = sd(data$birth_rate, na.rm = TRUE)), 
                color = "red", size = 1) +
  labs(title = "Histogram of Birth Rate with Normal Curve", x = "Birth Rate", y = "Density")

