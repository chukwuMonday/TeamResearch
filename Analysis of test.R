# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Load the data
data <- read.csv("C:/Users/Henry/Documents/Research/TeamResearch/russian_demography.csv")

# Check for missing values
print(colSums(is.na(data)))

# Ensure 'region' column exists
if (!("region" %in% colnames(data))) {
  stop("The dataset does not contain the 'region' column.")
}

# Select 10 unique regions
selected_regions <- unique(data$region)[1:10]  # Modify as needed to choose specific regions
print(paste("Selected regions:", paste(selected_regions, collapse = ", ")))

# Subset the data to include only the selected regions
data_10_regions <- data %>%
  filter(region %in% selected_regions)

# Check for missing values in the subset
print(colSums(is.na(data_10_regions)))

# Ensure there are no missing values in 'birth_rate' or 'region'
data_clean_10_regions <- data_10_regions %>%
  drop_na(birth_rate, region)

# Perform one-way ANOVA to test the difference in birth rates between the 10 regions
anova_result <- aov(birth_rate ~ region, data = data_clean_10_regions)
anova_summary <- summary(anova_result)

# Print the summary of the ANOVA result
print(anova_summary)

# Extract p-value from the ANOVA summary
p_value <- anova_summary[[1]][["Pr(>F)"]][1]

# Interpret the p-value
if (!is.na(p_value) && p_value < 0.05) {
  print("There is a significant difference in the mean birth rates between the selected regions.")
} else {
  print("There is no significant difference in the mean birth rates between the selected regions.")
}

# Create a boxplot of birth rates by the selected regions
ggplot(data_clean_10_regions, aes(x = region, y = birth_rate)) +
  geom_boxplot(fill = "skyblue", color = "darkblue", outlier.color = "red", outlier.shape = 16) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Boxplot of Birth Rates by Selected Regions", x = "Region", y = "Birth Rate")

# Create a histogram of birth rates
ggplot(data_clean_10_regions, aes(x = birth_rate)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(data_clean_10_regions$birth_rate, na.rm = TRUE), 
                            sd = sd(data_clean_10_regions$birth_rate, na.rm = TRUE)), 
                color = "red", size = 1) +
  labs(title = "Histogram of Birth Rates for Selected Regions with Normal Curve", 
       x = "Birth Rate", 
       y = "Density") +
  theme_minimal()

