# Read the dataset
data <- read.csv("~/Data Mining/Project1/COVID-19_cases_plus_census.csv")

# Remove specified columns
columns_to_remove <- c("pop_5_years_over", "speak_only_english_at_home", "speak_spanish_at_home", 
                       "speak_spanish_at_home_low_english", "pop_15_and_over", "pop_never_married", 
                       "pop_now_married", "pop_separated", "pop_widowed", "pop_divorced")
data <- data[, !(names(data) %in% columns_to_remove)]

# Impute missing values with median for numerical columns
data[, sapply(data, is.numeric)] <- lapply(data[, sapply(data, is.numeric)], function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))

# Print summary statistics of missing values after imputation
print("Missing values after imputation:")
print(colSums(is.na(data)))

# Load required libraries
library(caTools)
library(randomForest)

# Splitting data into train and test sets
set.seed(123) # Setting seed for reproducibility
split <- sample.split(data$deaths, SplitRatio = 0.7) 
train <- subset(data, split == TRUE)
test <- subset(data, split == FALSE)

# Fitting Random Forest to the train dataset 
classifier_RF <- randomForest(deaths ~ ., data = train, ntree = 500)

# Extracting important features
important_features <- importance(classifier_RF)
important_features <- as.data.frame(important_features)
important_features <- important_features[order(-important_features[, 1]), , drop = FALSE]

# Get the top 25 important features
top_25_features <- important_features[1:25, , drop = FALSE]

# Print important features
print("Top 25 Important Features:")
print(top_25_features)

# Plot histogram of importance scores
hist(top_25_features[,1], main = "Histogram of Top 36 Feature Importance Scores",
xlab = "Importance Score", ylab = "Frequency")

# Load required libraries
# install.packages("corrplot")
# Load required libraries
# Load required libraries
# Load required libraries
library(corrplot)
library(dplyr)

# Read your dataset (assuming it's named 'data')
# data <- read.csv("~/Data Mining/Project1/COVID-19_cases_plus_census.csv")

# Select the specified features from the dataset
selected_features <- data %>%
  select(high_school_diploma, high_school_including_ged, households_public_asst_or_food_stamps,
         no_cars, male_45_64_high_school, income_10000_14999, deaths, confirmed_cases,
         commute_45_59_mins, commute_60_more_mins, rent_burden_not_computed, 
         dwellings_3_to_4_units, commute_60_89_mins, rent_over_50_percent, 
         income_less_10000, female_80_to_84, employed_education_health_social, 
         children_in_single_female_hh, income_15000_19999, commute_90_more_mins, 
         housing_built_1939_or_earlier, female_85_and_over, no_car, 
         dwellings_50_or_more_units, aggregate_travel_time_to_work, commute_35_44_mins)

# Remove rows with missing values
selected_features <- na.omit(selected_features)

# Calculate the correlation matrix
correlation_matrix <- cor(selected_features)

# Increase the size of the output PNG file
png("correlation_matrix_plot.png", width = 1200, height = 1200)

# Plot the correlation matrix
corrplot(correlation_matrix, method = "circle", type = "upper", order = "hclust")

# Close the PNG device
dev.off()
