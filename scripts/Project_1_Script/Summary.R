# Load required packages
# install.packages("dplyr")
# install.packages("psych")

library(dplyr)
library(psych)
library(dplyr)

# Read your dataset (assuming it's named 'data')
data <- read.csv("~/Data Mining/Project1/COVID-19_cases_plus_census.csv")

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
# median_country_region_code <- median(imputed_data$country_region_code, na.rm = TRUE)
# imputed_data <- imputed_data %>%
#   mutate(country_region_code = ifelse(is.na(country_region_code), median_country_region_code, country_region_code))

# Display summary statistics of missing values after imputation
print("Missing values after imputation:")
print(colSums(is.na(imputed_data)))
# Specify the columns of interest
columns_of_interest <- c("high_school_diploma", "high_school_including_ged", "households_public_asst_or_food_stamps",
                         "no_cars", "male_45_64_high_school", "income_10000_14999", "deaths", "confirmed_cases",
                         "commute_45_59_mins", "commute_60_more_mins", "rent_burden_not_computed", 
                         "dwellings_3_to_4_units", "commute_60_89_mins", "rent_over_50_percent", 
                         "income_less_10000", "female_80_to_84", "employed_education_health_social", 
                         "children_in_single_female_hh", "income_15000_19999", "commute_90_more_mins", 
                         "housing_built_1939_or_earlier", "female_85_and_over", "no_car", 
                         "dwellings_50_or_more_units", "aggregate_travel_time_to_work", "commute_35_44_mins",
                         "state","total_pop","male_pop","female_pop","median_age","white_pop","black_pop","hispanic_pop",
                         "amerindian_pop","other_race_pop","gini_index")

# Generate summary for the specified columns
summary_stats <- summary(data[columns_of_interest])
print(summary_stats)

# Selecting columns of interest
columns_of_interest <- c("total_pop", "male_pop", "female_pop", "median_age", 
                         "white_pop", "black_pop", "hispanic_pop", "amerindian_pop", 
                         "other_race_pop", "gini_index", "income_10000_14999", 
                         "deaths", "confirmed_cases", "aggregate_travel_time_to_work", 
                         "commute_35_44_mins", "commute_45_59_mins", "commute_60_more_mins", 
                         "income_less_10000", "income_15000_19999", "commute_90_more_mins")

# Read the original data file
# original_data <- read.csv("original_data.csv")

# Select only the columns of interest
selected_data <- imputed_data[, columns_of_interest]

# Write the selected data to a new CSV file
write.csv(selected_data, "selected_features.csv", row.names = FALSE)
print(selected_data)
# Download the CSV file to local machine
# fileURL <- "selected_features.csv"
# download.file(fileURL, destfile = "selected_features.csv", method = "auto")
