library(dplyr)
library(ggplot2)

# Read the data
demography_data <- read.csv("C:/Users/manichandu/Downloads/russian_demography.csv")

# Clean the data by removing rows with missing values in 'birth_rate' and 'region'
demography_data_clean <- demography_data %>%
  drop_na(birth_rate, region)

# Create a histogram of birth rates
ggplot(demography_data_clean, aes(x = birth_rate)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "darkblue", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Histogram of Birth Rates", x = "Birth Rate", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

