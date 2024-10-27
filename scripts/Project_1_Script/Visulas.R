# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Read the dataset
data <- read.csv("~/Data Mining/Project1/Global_Mobility_Report.csv")

# Convert 'date' column to Date format
data$date <- as.Date(data$date)

# Select relevant columns for time series plots
selected_data <- data %>%
  select(date, 
         retail_and_recreation_percent_change_from_baseline,
         grocery_and_pharmacy_percent_change_from_baseline,
         parks_percent_change_from_baseline,
         transit_stations_percent_change_from_baseline,
         workplaces_percent_change_from_baseline,
         residential_percent_change_from_baseline)

# Reshape the data for plotting
selected_data_long <- selected_data %>%
  pivot_longer(cols = -date, names_to = "activity", values_to = "percent_change")

# Plot time series for each activity
ggplot(selected_data_long, aes(x = date, y = percent_change, color = activity)) +
  geom_line() +
  labs(title = "Percentage Changes from Baseline Over Time",
       x = "Date",
       y = "Percentage Change",
       color = "Activity") +
  theme_minimal()

