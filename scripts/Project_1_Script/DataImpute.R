# Load required packages
install.packages("dplyr")
library(dplyr)

# Read your dataset (assuming it's named 'data')
data <- read.csv("~/Data Mining/Project1/COVID-19_cases_TX.csv")

# Display summary statistics of missing values before imputation
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
print(summary(imputed_data))