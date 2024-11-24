library(dplyr)
library(ggplot2)
library(tidyr)
library(car)

# Read the data
demography_data <- read.csv("C:/Users/manichandu/Downloads/russian_demography.csv")

# Check for missing values
colSums(is.na(demography_data))

# Clean the data by removing rows with missing values in 'birth_rate' and 'region'
demography_data_clean <- demography_data %>%
  drop_na(birth_rate, region)

# Perform a normality test for birth_rate (Shapiro-Wilk Test)
shapiro_test <- shapiro.test(demography_data_clean$birth_rate)
print(shapiro_test)

# Check if the birth_rate distribution is normal (p-value > 0.05 means normal distribution)
if (shapiro_test$p.value > 0.05) {
  print("The birth_rate data appears to follow a normal distribution.")
} else {
  print("The birth_rate data does not follow a normal distribution.")
}

# Create a density plot (bell curve) of birth rates for the entire dataset
ggplot(demography_data_clean, aes(x = birth_rate)) +
  geom_density(fill = "skyblue", color = "darkblue") +
  labs(title = "Density Plot of Birth Rates", x = "Birth Rate", y = "Density") +
  theme_minimal()

# Optionally: Create density plots by region to compare distributions across regions
ggplot(demography_data_clean, aes(x = birth_rate, fill = region)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Birth Rates by Region", x = "Birth Rate", y = "Density") +
  theme_minimal() +
  theme(legend.title = element_blank())

# Optional: Add a normal distribution curve for comparison
ggplot(demography_data_clean, aes(x = birth_rate)) +
  geom_density(fill = "skyblue", color = "darkblue") +
  stat_function(fun = dnorm, args = list(mean = mean(demography_data_clean$birth_rate), sd = sd(demography_data_clean$birth_rate)), color = "red", size = 1) +
  labs(title = "Density Plot with Normal Distribution Curve", x = "Birth Rate", y = "Density") +
  theme_minimal()




