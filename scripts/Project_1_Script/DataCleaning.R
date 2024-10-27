# Load required libraries
library(randomForest)

# Function to handle missing values by imputing median
impute_median <- function(column) {
  median_value <- median(column, na.rm = TRUE)
  return(ifelse(is.na(column), median_value, column))
}

# Load the dataset
your_dataset <- read.csv("~/Data Mining/Project1/COVID-19_cases_plus_census.csv")

# Handle missing values by imputing median for numerical columns
your_dataset[, sapply(your_dataset, is.numeric)] <- lapply(your_dataset[, sapply(your_dataset, is.numeric)], impute_median)

# Separate features and target variable
features <- your_dataset[, -ncol(your_dataset)]  # Excluding the target variable
target <- your_dataset[, ncol(your_dataset)]     # Target variable

# Train a random forest model
set.seed(123)  # For reproducibility
rf_model <- randomForest(x = features, y = target, ntree = 500)

# Extract feature importance
importance <- importance(rf_model)

# Print or visualize important features
print(importance)

# Plot variable importance
varImpPlot(rf_model)
