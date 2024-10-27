# Load necessary libraries
library(dplyr)

# Read the dataset
data <- read.csv("~/Data Mining/Project1/Global_Mobility_Report.csv")

# Step 1: Find missing values
missing_values <- colSums(is.na(data))

print("Missing values before imputation:")
print(colSums(is.na(data)))

# Define a function to perform median imputation for numerical columns
impute_median <- function(column) {
  if (is.numeric(column)) {
    median_value <- median(column, na.rm = TRUE)
    return(ifelse(is.na(column), median_value, column))
  } else {
    return(column)
  }
}

# Perform median imputation for numerical columns separately and add imputed columns to the dataframe
num_cols <- sapply(data, is.numeric)
imputed_data <- data %>%
  mutate_at(vars(names(data)[num_cols]), impute_median)

# Perform median imputation for the 'country_region_code' column separately
median_country_region_code <- median(imputed_data$country_region_code, na.rm = TRUE)
imputed_data <- imputed_data %>%
  mutate(country_region_code = ifelse(is.na(country_region_code), median_country_region_code, country_region_code))

# Display summary statistics of missing values after imputation
print("Missing values after imputation:")
print(colSums(is.na(imputed_data)))

# Step 3: Normalize data
numeric_cols <- sapply(imputed_data, is.numeric)
data_normalized <- imputed_data
data_normalized[, numeric_cols] <- scale(imputed_data[, numeric_cols])

# Step 4: Check for duplicates
duplicates <- duplicated(data_normalized)

# Step 5: Print statistical summary
print(summary(data_normalized))

# Step 6: Plot outliers
# You can plot outliers using any suitable method or visualization library. For example:
# Assuming you want to plot boxplots for each numeric column
boxplot(data_normalized[, numeric_cols], outline = TRUE)
# Load necessary libraries
library(ggplot2)

# Create a dataframe with the features and their values
data <- data.frame(
  category = c(
    "Retail & Recreation", 
    "Grocery & Pharmacy", 
    "Parks", 
    "Transit Stations", 
    "Workplaces", 
    "Residential"
  ),
  percent_change = c(
    "retail_and_recreation_percent_change_from_baseline",
    "grocery_and_pharmacy_percent_change_from_baseline",
    "parks_percent_change_from_baseline",
    "transit_stations_percent_change_from_baseline",
    "workplaces_percent_change_from_baseline",
    "residential_percent_change_from_baseline"
  )
)

# Plot a bar graph
ggplot(data, aes(x = category, y = percent_change)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Percentage Change from Baseline by Category",
       x = "Category",
       y = "Percent Change") +
  theme_minimal()
