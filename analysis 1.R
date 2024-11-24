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

# Perform one-way ANOVA to test the difference in birth rates between regions
anova_result <- aov(birth_rate ~ region, data = demography_data_clean)

# Print the summary of the ANOVA result
anova_summary <- summary(anova_result)
print(anova_summary)

# Extract p-value from the ANOVA summary
p_value <- anova_summary[[1]]$`Pr(>F)`[1]

# Interpret the p-value
if (p_value < 0.05) {
  print("There is a significant difference in the mean birth rates between regions.")
} else {
  print("There is no significant difference in the mean birth rates between regions.")
}

# Create a boxplot of birth rates by region
ggplot(demography_data_clean, aes(x = region, y = birth_rate)) +
  geom_boxplot(fill = "skyblue", color = "darkblue", outlier.color = "red", outlier.shape = 16) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Boxplot of Birth Rates by Region", x = "Region", y = "Birth Rate")


