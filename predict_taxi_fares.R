# Loading necessary libraries
library(tidyverse)
library(ggmap)
library(viridis)
library(lubridate)
library(tree)
library(randomForest)

# Load dataset
# Replace 'your_dataset.csv' with the path to your dataset
data <- read.csv("C:/Users/darla/OneDrive/Documents/R Programs/taxi.csv")

# Data preprocessing
# Convert datetime columns to POSIXct
data$pickup_datetime <- ymd_hms(data$pickup_datetime)


# Extract temporal features
data$pickup_hour <- hour(data$pickup_datetime)
data$pickup_day <- wday(data$pickup_datetime, label = TRUE)

# Remove rows with missing or erroneous values
data <- na.omit(data)
data <- data[data$fare_amount > 0, ]

ggmap(nyc_map) +
  geom_point(data = data, aes(x = pickup_longitude, y = pickup_latitude),
             color = "blue", alpha = 0.5) +
  labs(title = "Taxi Pickup Locations")

# Train-Test Split
set.seed(123)
train_indices <- sample(1:nrow(data), 0.8 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Decision Tree Model
tree_model <- tree(fare_amount ~ trip_time_in_secs + pickup_datetime + pickup_longitude + pickup_latitude, data = train_data)
summary(tree_model)

# Random Forest Model
rf_model <- randomForest(fare_amount ~ trip_time_in_secs + pickup_datetime + pickup_longitude + pickup_latitude, data = train_data, ntree = 100, importance = TRUE)
print(rf_model)

# Predictions and Validation
tree_predictions <- predict(tree_model, test_data)
rf_predictions <- predict(rf_model, test_data)

# Calculate RMSE for both models
tree_rmse <- sqrt(mean((tree_predictions - test_data$fare_amount)^2))
rf_rmse <- sqrt(mean((rf_predictions - test_data$fare_amount)^2))

cat("Decision Tree RMSE:", tree_rmse, "\n")
cat("Random Forest RMSE:", rf_rmse, "\n")

# Visualize predictions vs actual fares
test_data <- test_data %>%
  mutate(tree_pred = tree_predictions, rf_pred = rf_predictions)

ggplot(test_data, aes(x = fare_amount)) +
  geom_point(aes(y = tree_pred, color = "Tree")) +
  geom_point(aes(y = rf_pred, color = "Random Forest")) +
  labs(title = "Predicted vs Actual Taxi Fares", y = "Predicted Fare", color = "Model")

# Geospatial Heatmap
heatmap_data <- data %>%
  group_by(pickup_latitude, pickup_longitude) %>%
  summarize(mean_fare = mean(fare_amount))

