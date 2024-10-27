# Load necessary libraries
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

# Plot outliers
numeric_cols <- sapply(normalized_data, is.numeric)
numeric_data <- normalized_data[, numeric_cols]
boxplot_data <- stack(numeric_data)

# Create total commute time variable
total_commute_time <- mutate(your_dataset, total_commute_time = commute_60_89_mins + commute_90_more_mins)

# Scatter plot of commute time vs aggregate total time
scatter_plot <- ggplot(total_commute_time, aes(x = total_commute_time, y = aggregate_travel_time_to_work)) +
  geom_point() +
  labs(title = "Scatter Plot of Commute Time vs Aggregate Total Time",
       x = "Commute Time",
       y = "Aggregate Total Time")

# Print results
print(head(normalized_data))
print("Number of missing values for each feature:")
print(missing_values)
print("Duplicates in each feature:")
print(duplicates)
print(summary_data)
print(scatter_plot)
# Plot boxplots for each numeric column to detect outliers
columns <- c("deaths", "confirmed_cases")

# Create a dataframe with the selected columns
boxplot_data <- normalized_data[, columns]

# Plot outliers for each column
for (col in columns) {
  ggplot(boxplot_data, aes(y = .data[[col]])) +
    geom_boxplot() +
    labs(title = paste("Outliers Plot for", col),
         x = "Columns",
         y = col) +
    theme_minimal()
}

# Plot Total Population vs Confirmed Cases
total_pop_cases <- ggplot(normalized_data, aes(x = total_pop, y = confirmed_cases)) +
  geom_point() +  # Use geom_point for scatter plot
  labs(title = "Total Population vs Confirmed Cases",
       x = "Total Population",
       y = "Confirmed Cases")

# Print the plot
print(total_pop_cases)

deaths_total_pop <- ggplot(normalized_data, aes(x = total_pop, y = deaths)) +
  geom_point() +
  labs(title = "Deaths vs Total Population",
       x = "Total Population",
       y = "Deaths")

print(deaths_total_pop)

confirmed_cases_total_pop <- ggplot(normalized_data, aes(x = total_pop, y = confirmed_cases)) +
  geom_point() +
  labs(title = "Confirmed Cases vs Total Population",
       x = "Total Population",
       y = "Confirmed Cases")

print(confirmed_cases_total_pop)

# Load necessary libraries
# Load necessary libraries
# Calculate Sigma-Tukey residuals manually
library(ggplot2)

# Assuming your dataset is named 'your_dataset' and contains columns for 'county_name' and 'confirmed_cases'

# Calculate Sigma-Tukey residuals manually
sigma_tukey_residuals <- with(your_dataset, {
  fit <- lm(confirmed_cases ~ county_name, data = your_dataset)
  residuals(fit) / summary(fit)$sigma
})

# Create a dataframe for plotting
df <- data.frame(county_name = your_dataset$county_name, sigma_tukey_residuals)

# Plot histogram of Sigma-Tukey residuals
histogram <- ggplot(df, aes(x = sigma_tukey_residuals)) +
  geom_histogram(aes(y = ..density..),bins = 50, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Sigma-Tukey Residuals for Confirmed Cases",
       x = "Sigma-Tukey Residuals",
       y = "Frequency")

# Print the histogram
print(histogram)

library(ggplot2)

# Calculate Sigma-Tukey residuals manually
sigma_tukey_residuals <- with(normalized_data, {
  fit <- lm(confirmed_cases ~ county_name, data = normalized_data)
  residuals(fit) / summary(fit)$sigma
})

# Create a dataframe for plotting
df <- data.frame(county_name = normalized_data$county_name, sigma_tukey_residuals)

# Plot histogram of Sigma-Tukey residuals
histogram <- ggplot(df, aes(x = sigma_tukey_residuals)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = "skyblue", color = "black", binwidth = 0.5) +
  labs(title = "Histogram of Sigma-Tukey Residuals for Confirmed Cases",
       x = "Sigma-Tukey Residuals",
       y = "Density") +
  scale_x_continuous(breaks = seq(0, 50, by = 10)) +  # Set breaks on x-axis
  theme_minimal()

# Print the histogram
print(histogram)

library(dplyr)
library(ggplot2)

# Define the demographic features
demographic_features <- c("total_pop", "male_pop", "female_pop",
                          "white_pop", "black_pop",
                          "hispanic_pop", "amerindian_pop", "other_race_pop")

# Calculate the sum of deaths for each demographic feature
deaths_sum <- sapply(demographic_features, function(feature) {
  sum(normalized_data$deaths * normalized_data[[feature]])
})

# Create a dataframe with the sums
deaths_data <- data.frame(feature = demographic_features, deaths_sum)

# Plot the pie chart
pie_chart <- ggplot(deaths_data, aes(x = "", y = deaths_sum, fill = feature)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Pie Chart of Deaths by Demographic Features",
       fill = "Feature") +
  theme_minimal() +
  theme(legend.position = "right")

# Print the pie chart
print(pie_chart)

library(dplyr)
library(ggplot2)

# Define the demographic features
demographic_features <- c("total_pop", "male_pop", "female_pop",
                           "white_pop", "black_pop",
                          "hispanic_pop", "amerindian_pop", "other_race_pop")

# Calculate the sum of confirmed cases for each demographic feature
cases_sum <- sapply(demographic_features, function(feature) {
  sum(normalized_data$confirmed_cases * normalized_data[[feature]])
})

# Create a dataframe with the sums
cases_data <- data.frame(feature = demographic_features, cases_sum)

# Plot the pie chart
pie_chart <- ggplot(cases_data, aes(x = "", y = cases_sum, fill = feature)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Pie Chart of Confirmed Cases by Demographic Features",
       fill = "Feature") +
  theme_minimal() +
  theme(legend.position = "right")

# Print the pie chart
print(pie_chart)

library(ggplot2)

# Assuming your dataset is named 'your_dataset' and contains columns for 'gini_index' and 'state'

# Plot density plot of gini_index vs state
density_plot <- ggplot(your_dataset, aes(x = gini_index, fill = state)) +
  geom_density(alpha = 1) +
  labs(title = "Density Plot of Gini Index by State",
       x = "Gini Index",
       y = "Density",
       fill = "State") +
  theme_minimal()

# Print the density plot
print(density_plot)

library(ggplot2)

# Assuming your dataset is named 'your_dataset' and contains columns for 'county_name' and 'male_pop'
library(ggrepel)
# Aggregate male_pop cases by county_name
cases_TX <- your_dataset%>% filter(state == "CA")
dim(cases_TX)
ggplot(cases_TX, mapping = aes(x = confirmed_cases, y = deaths, label = county_name)) + 
  geom_smooth(method = lm) +
  geom_point(mapping = aes(size = total_pop), color = "grey") + 
  geom_text_repel(data = subset(cases_TX, deaths >= 1000)) 

