# Read the CSV file into a data frame
library(readr)
data <- read.csv("~/Data Mining/Project1/Mixed.csv")
# Load the dplyr package
library(dplyr)
print(head(data,10))
cases_TX <- data %>% filter(state == "CA")
print(dim(cases_TX))
print(summary(cases_TX[,1:10]))
library(ggplot2)
ggplot(cases_TX, mapping = aes(confirmed_cases)) + geom_histogram(bins = 20)
ggplot(cases_TX, mapping = aes(x = confirmed_cases, y = deaths, size = total_pop)) + geom_point()
cases_TX_select <- cases_TX %>% filter(confirmed_cases > 1000) %>% 
  arrange(desc(confirmed_cases)) %>%    
  select(county_name, confirmed_cases, deaths, total_pop, median_income)
cases_TX_select <- cases_TX_select %>% mutate(
  cases_per_1000 = confirmed_cases/total_pop*1000, 
  deaths_per_1000 = deaths/total_pop*1000, 
  death_per_case = deaths/confirmed_cases)

# print(head(cases_TX_select))
# print(summary(cases_TX_select$median_income))
# install.packages("ggcorrplot")
library(ggcorrplot)
# Install the stringr package if not already installed
# install.packages("stringr")

# Load the stringr package
library(stringr)

cor_TX <- cor(cases_TX_select[,-1])
ggcorrplot(cor_TX, p.mat = cor_pmat(cases_TX_select[,-1]), insig = "blank", hc.order = TRUE)
counties <- as_tibble(map_data("county"))
counties_TX <- counties %>% dplyr::filter(region == "california") %>% rename(c(county = subregion))

cases_TX <- cases_TX_select %>% mutate(county = county_name %>% str_to_lower() %>% str_replace('\\s+county\\s*$', ''))
counties_TX <- counties_TX %>% left_join(cases_TX %>% 
                                           select(c(county, cases_per_1000, deaths_per_1000, death_per_case)))
ggplot(counties_TX, aes(long, lat, label = county)) + 
  geom_polygon(aes(group = group, fill = cases_per_1000)) +
  # geom_text_repel(data = counties_TX %>% filter(complete.cases(.)) %>% group_by(county) %>% 
  #    summarize(long = mean(long), lat = mean(lat)) %>% mutate(county = str_to_title(county))) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "COVID-19 Cases per 1000 People", subtitle = "Only counties reporting 100+ cases")

# Assuming your dataset is already loaded and named 'data'

# Select only the columns of interest
# Assuming your dataset is already loaded and named 'data'

# Select only the columns of interest
selected_columns <- c("deaths", "confirmed_cases", "white_pop", "black_pop", "hispanic_pop", "amerindian_pop", "other_race_pop")
selected_data <- data[, selected_columns]

# Calculate the correlation matrix
correlation_matrix <- cor(selected_data)
reversed_row_labels <- rev(selected_columns)

# Reverse the order of rows in the correlation matrix
reversed_correlation_matrix <- correlation_matrix[nrow(correlation_matrix):1, ]
# Create the correlation heatmap with y-axis labels on the left side
heatmap(correlation_matrix,
        Colv = NA, Rowv = NA,
        # col = colorRampPalette(c("blue", "white", "red"))(100),
        scale = "none",
        main = "Demographic Correlation Heatmap",
        # xlab = "Features",
        # ylab = "Features",
        # labRow = selected_columns,  # Specify row labels
        labRow = reversed_row_labels,
        cexRow = 0.8,  # Adjust row label size if needed
        cexCol = 0.8,  # Adjust column label size if needed
)
# Assuming your dataset is already loaded and named 'data'

# Select the specified columns
selected_columns <- c("income_10000_14999", "deaths", "confirmed_cases",
                      "commute_35_44_mins", "commute_45_59_mins", "commute_60_more_mins", "income_less_10000",
                      "income_15000_19999", "commute_90_more_mins")

selected_data <- data[, selected_columns]

# Calculate the correlation matrix
correlation_matrix <- cor(selected_data)

# Plot the heatmap

heatmap(correlation_matrix,
        Colv = NA, Rowv = NA,
        # col = colorRampPalette(c("blue", "white", "red"))(100),
        scale = "none",
        main = "Correlation 
        Heatmap of Socioeconomic and Health Indicators",
        # xlab = "Features",
        # ylab = "Features",
        cexRow = 0.7,  # Adjust row label size if needed
        cexCol = 0.7,  # Adjust column label size if needed
)
png("heatmap.png", width = 5, height = 5, units = "in", res = 300)

