library(dplyr)
library(ggplot2)

# Define the dataset path
dataset_path <- "~/Data Mining/Project1/COVID-19_cases_plus_census.csv"

# Read the dataset
your_dataset <- read.csv(dataset_path)

# Specify the features to be normalized
features <- c(
  "high_school_diploma", "high_school_including_ged",
  "households_public_asst_or_food_stamps", "no_cars",
  "male_45_64_high_school", "income_10000_14999",
  "deaths", "confirmed_cases",
  "commute_45_59_mins", "commute_60_more_mins",
  "rent_burden_not_computed", "dwellings_3_to_4_units",
  "commute_60_89_mins", "rent_over_50_percent",
  "income_less_10000", "female_80_to_84",
  "employed_education_health_social", "children_in_single_female_hh",
  "income_15000_19999", "commute_90_more_mins",
  "housing_built_1939_or_earlier", "female_85_and_over",
  "dwellings_50_or_more_units", "aggregate_travel_time_to_work",
  "commute_35_44_mins", "total_pop",
  "male_pop", "female_pop",
  "median_age", "white_pop",
  "black_pop", "hispanic_pop",
  "amerindian_pop", "other_race_pop",
  "gini_index"
)

# Normalize the specified features in the dataset
normalized_data <- your_dataset %>%
  mutate(across(all_of(features), scale))

# Check for missing values in the specified features
missing_values <- sapply(normalized_data[, features], function(x) sum(is.na(x)))

# Impute missing values with mean for the specified features
for (feature in features) {
  normalized_data[[feature]][is.na(normalized_data[[feature]])] <- mean(normalized_data[[feature]], na.rm = TRUE)
}

# Check for duplicates in the specified features
duplicates <- duplicated(normalized_data[, features])

# Print summary
summary_data <- summary(normalized_data)
# Principal Component Analysis (PCA)
# Perform PCA
# Perform PCA
pca_result <- prcomp(normalized_data[, features], scale. = TRUE)

# Scree plot
# Perform PCA
pca_result <- prcomp(normalized_data[, features], scale. = TRUE)

# Scree plot
scree_data <- data.frame(PC = 1:length(pca_result$sdev), Proportion_of_Variance = pca_result$sdev^2 / sum(pca_result$sdev^2))

ggplot(scree_data, aes(x = PC, y = Proportion_of_Variance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = round(Proportion_of_Variance, 2)), vjust = -0.5) +
  labs(title = "Scree Plot", x = "Principal Component", y = "Proportion of Variance Explained")

# Biplot
library(factoextra)
fviz_pca_ind(pca_result, geom.ind = "point", col.ind = "blue", repel = TRUE) +
  labs(title = "Biplot")

# Extract PCA scores
pca_scores <- as.data.frame(pca_result$x)

# Plot histograms for each principal component
par(mfrow = c(3, 3))  # Adjust the layout as needed
for (i in 1:min(ncol(pca_scores), 9)) {
  hist(pca_scores[, i], main = paste("PC", i), xlab = paste("PC", i), col = "skyblue", border = "black")
}

