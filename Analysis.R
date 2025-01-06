# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(car)
# Install dplyr if not already installed
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}

# Load dplyr
library(dplyr)


# Load the data
data <- read.csv("C:/Users/Henry/Documents/Research/TeamResearch/russian_demography.csv")

# Check for missing values
print(colSums(is.na(data)))

# Ensure birth_rate and urbanization columns exist
if (!("birth_rate" %in% colnames(data)) || !("urbanization" %in% colnames(data))) {
  stop("The dataset does not contain the required columns: 'birth_rate' or 'urbanization'.")
}

# Correlation between birth_rate and urbanization
if (!any(is.na(data$birth_rate)) && !any(is.na(data$urbanization))) {
  cor_test <- cor.test(data$birth_rate, data$urbanization, method = "pearson")
  print(cor_test)
} else {
  print("Missing values in 'birth_rate' or 'urbanization' columns. Correlation not performed.")
}

# Categorize urbanization and birth_rate
data <- data %>%
  mutate(
    urban_category = ifelse(urbanization > 50, "Urbanized", "Less Urbanized"),
    birth_rate_category = ifelse(
      birth_rate > median(birth_rate, na.rm = TRUE), 
      "High Birth Rate", "Low Birth Rate"
    )
  )

# Create a contingency table
contingency_table <- table(data$urban_category, data$birth_rate_category)
print(contingency_table)

# Perform Chi-Square Test
if (all(contingency_table > 0)) {
  chi2_result <- chisq.test(contingency_table)
  print(chi2_result)
} else {
  print("Chi-Square Test cannot be performed due to zero counts in the contingency table.")
}

# Ensure 'region' column exists
if (!("region" %in% colnames(data))) {
  stop("The dataset does not contain the 'region' column.")
}

# Clean the data by removing rows with missing values in 'birth_rate' and 'region'
demography_data_clean <- data %>%
  drop_na(birth_rate, region)

# Perform one-way ANOVA to test the difference in birth rates between regions
anova_result <- aov(birth_rate ~ region, data = demography_data_clean)
anova_summary <- summary(anova_result)

# Print the summary of the ANOVA result
print(anova_summary)

# Extract p-value from the ANOVA summary
p_value <- anova_summary[[1]][["Pr(>F)"]][1]

# Interpret the p-value
if (!is.na(p_value) && p_value < 0.05) {
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

# Histogram with normal curve overlay
if ("birth_rate" %in% colnames(data)) {
  ggplot(data, aes(x = birth_rate)) +
    geom_histogram(aes(y = ..density..), binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
    stat_function(fun = dnorm, 
                  args = list(mean = mean(data$birth_rate, na.rm = TRUE), 
                              sd = sd(data$birth_rate, na.rm = TRUE)), 
                  color = "red", size = 1) +
    labs(title = "Histogram of Birth Rate with Normal Curve", x = "Birth Rate", y = "Density")
} else {
  print("The 'birth_rate' column does not exist in the dataset.")
}

