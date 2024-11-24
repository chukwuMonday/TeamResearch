# Load necessary libraries
library(dplyr)
library(ggplot2)

# Read the data
demography_data <- read.csv("C:/Users/manichandu/Downloads/russian_demography.csv")

# Clean the data by removing rows with missing values in 'birth_rate', 'urbanization', or 'npg'
demography_data_clean <- demography_data %>%
  drop_na(birth_rate, urbanization, npg)

# Optionally, filter extreme 'npg' values if necessary
demography_data_clean <- demography_data_clean %>%
  filter(npg > 0)  # Example: Filter out rows where 'npg' is 0 or negative

# Create the bubble chart for birth_rate vs urbanization with bubble size as npg
bubble_chart <- ggplot(demography_data_clean, aes(x = urbanization, y = birth_rate, size = npg, color = region)) +
  geom_point(alpha = 0.7) + # Add bubbles with transparency
  theme_minimal() + # Apply minimal theme
  labs(title = "Bubble Chart: Birth Rate vs Urbanization", 
       x = "Urbanization", 
       y = "Birth Rate") +
  scale_size_continuous(range = c(4, 20)) +  # Increase bubble size range (zoom in)
  theme(legend.position = "right") +  # Position the legend to the right
  coord_cartesian(xlim = c(30, 75), ylim = c(5, 30))  # Zoom into specific area (adjust as needed)

# Display the plot in the interactive environment (RStudio)
print(bubble_chart)

# Optionally, save the plot as a larger image (e.g., PNG) to avoid cutting off
ggsave("bubble_chart_zoomed_rstudio.png", plot = bubble_chart, width = 10, height = 8, dpi = 300)

