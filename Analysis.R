
library(ggplot2)


# Load the data
data <- read.csv("C:/Users/hency/Documents/TeamResearch/russian_demography.csv")

# Correlation between birth_rate and urbanization
cor_test <- cor.test(data$birth_rate, data$urbanization, method = "pearson")
print(cor_test)


# Subset for the first 10 regions
sample_regions <- unique(data$region)[1:10]
filtered_data <- subset(data, region %in% sample_regions)

# Perform ANOVA
anova_result <- aov(birth_rate ~ region, data = filtered_data)
summary(anova_result)



kruskal_test <- kruskal.test(birth_rate ~ region, data = filtered_data)
print(kruskal_test)


# Categorize urbanization and birth_rate
data$urban_category <- ifelse(data$urbanization > 50, "Urbanized", "Less Urbanized")
birth_rate_median <- median(data$birth_rate)
data$birth_rate_category <- ifelse(data$birth_rate > birth_rate_median, "High Birth Rate", "Low Birth Rate")

# Create contingency table
contingency_table <- table(data$urban_category, data$birth_rate_category)
print(contingency_table)

# Perform Chi-Square Test
chi2_result <- chisq.test(contingency_table)
print(chi2_result)



# Load necessary libraries
library(ggplot2)

# Subset data to a manageable number of regions (e.g., first 10 regions)
sample_regions <- unique(data$region)[1:10]
filtered_data <- subset(data, region %in% sample_regions)

# Create a faceted histogram
ggplot(filtered_data, aes(x = birth_rate)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  facet_wrap(~ region, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Histogram of Birth Rates by Region",
    x = "Birth Rate",
    y = "Frequency"
  )

