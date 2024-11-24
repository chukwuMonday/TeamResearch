library(dplyr)
library(ggplot2)

# Read the data
demography_data <- read.csv("C:/Users/manichandu/Downloads/russian_demography.csv")

# Clean the data by removing rows with missing values in 'birth_rate' and 'region'
demography_data_clean <- demography_data %>%
  drop_na(birth_rate, region)

# Create the time series plot for birth_rate by region
time_series_plot <- ggplot(demography_data_clean, aes(x = year, y = birth_rate, color = region)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Time Series Plot of Birth Rates by Region", 
       x = "Year", 
       y = "Birth Rate") +
  theme(legend.title = element_blank(), # Remove legend title
        legend.position = "right") + # Position the legend to the right
  scale_color_manual(values = rainbow(length(unique(demography_data_clean$region)))) # Customize colors

# Save the plot with a larger size
ggsave("time_series_plot.png", plot = time_series_plot, width = 16, height = 14, dpi = 300)

