# Load necessary libraries
library(dplyr)

# Read the dataset
data <- read.csv("~/Data Mining/Project1/COVID-19_cases_TX.csv")

# Step 1: Find missing values
missing_values <- colSums(is.na(data))
print(missing_values)

# Step 2: Impute missing values with mean for numeric columns and most frequent value for non-numeric columns
impute_most_frequent <- function(x) {
  if (is.numeric(x)) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
  } else {
    levels <- levels(x)
    x[is.na(x)] <- levels[which.max(tabulate(match(x, levels)))]
  }
  return(x)
}

data_imputed <- data %>%
  mutate(across(everything(), impute_most_frequent))

# Step 3: Normalize data
numeric_cols <- sapply(data_imputed, is.numeric)
data_normalized <- data_imputed
data_normalized[, numeric_cols] <- scale(data_imputed[, numeric_cols])

# Step 4: Check for duplicates
duplicates <- duplicated(data_normalized)

# Step 5: Print statistical summary
print(summary(data_normalized))

# Step 6: Plot outliers
# You can plot outliers using any suitable method or visualization library. For example:
# Assuming you want to plot boxplots for each numeric column
boxplot(data_normalized[, numeric_cols], outline = TRUE)

# Step 7: Plotting the data
# Check data type of confirmed_cases column
# Check if confirmed_cases and deaths columns exist in the dataset
# Check if confirmed_cases and deaths columns exist in the dataset
# Check if confirmed_cases and deaths columns exist in the dataset
# Check if confirmed_cases and deaths columns exist in the dataset
if ("confirmed_cases" %in% colnames(data_normalized) && "deaths" %in% colnames(data_normalized)) {
  # Remove rows with NA values in the columns used for plotting
  plot_data <- na.omit(data_normalized[c("confirmed_cases", "deaths")])
  
  # Sort data by confirmed_cases for smoother curve
  plot_data <- plot_data[order(plot_data$confirmed_cases), ]
  
  # Plot deaths as a curve graph against confirmed_cases
  plot(plot_data$confirmed_cases, plot_data$deaths,
       main = "Curve Graph: Confirmed Cases vs Deaths",
       xlab = "Confirmed Cases", ylab = "Deaths",
       type = "l", col = "blue", lwd = 2)  # Use a line plot (curve) with thicker line
} else {
  print("confirmed_cases or deaths column not found in the dataset.")
}
# Load necessary libraries
# Load necessary libraries
# Load necessary libraries
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Assuming 'data_normalized' is your dataset containing columns 'date', 'confirmed_cases', and 'deaths'

# Convert 'date' column to Date type
data_normalized$date <- as.Date(data_normalized$date)

# Create a time series plot
library(ggplot2)

# Assuming 'data_normalized' is your dataset containing columns 'date', 'confirmed_cases', and 'deaths'

# Convert 'date' column to Date type
data_normalized$date <- as.Date(data_normalized$date)

# Create a simple line plot for confirmed cases
ggplot(data_normalized, aes(x = date)) +
  geom_line(aes(y=confirmed_cases),color = "blue") +
  labs(title = "Time Series Plot of Confirmed Cases",
       x = "Date", y = "Confirmed Cases") +
  theme_minimal()

# Create a simple line plot for deaths
ggplot(data_normalized, aes(x = date, y = deaths)) +
  geom_line(color = "red") +
  labs(title = "Time Series Plot of Deaths",
       x = "Date", y = "Deaths") +
  theme_minimal()



