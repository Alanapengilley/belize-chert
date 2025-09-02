#APPLY LOF to determine if artifacts are considered outliers or inliers compared to geological data.
#Outliers are removed from analysis, as considered to not be correlated to current geological database. 


# Load necessary libraries
library(ggplot2)
library(dbscan)
library(dplyr)
library(scale)
library(readxl)
library(Rtsne)

##LOAD IN DATA 
#geological data set, outliers removed 
train_data <- read.csv("LAICPMS Data/subset_data_optimal_features.csv")
#artifact data set
artifacts <- read.csv("LAICPMS Data/artifacts_optimal_features.csv") 
test_data <- artifacts


# Ensure the data is numeric
train_data <- train_data[sapply(train_data, is.numeric)]
test_data <- test_data[sapply(test_data, is.numeric)]


# Train the LOF model
lof_model <- lof(train_data, minPts = 20)

# Get LOF scores for test data
test_lof_scores <- lof(test_data, minPts = 20)

# Set a threshold for classification (e.g., median LOF score or a custom threshold)
threshold <- median(lof_model)

# Classify test data based on LOF scores
test_predictions <- ifelse(test_lof_scores > threshold, -1, 1)  # -1 for outliers, 1 for inliers

# Print results
print(test_predictions)

# Run t-SNE to reduce dimensions of the test data to 2D for visualization
set.seed(42)  # For reproducibility
tsne_result <- Rtsne(test_data, dims = 2, pca = TRUE, perplexity = 15, max_iter = 1000)

# Create a data frame with the t-SNE results and LOF predictions
test_results <- data.frame( 
  tsne1 = tsne_result$Y[, 1],  # First t-SNE component
  tsne2 = tsne_result$Y[, 2],  # Second t-SNE component
  Prediction = factor(test_predictions)  # LOF prediction (1 = inlier, -1 = outlier)
)

print(test_results)

# Add the predictions as a new column to the original test dataset
artifacts$Prediction <- test_predictions
# Add the LOF predictions as a new column to the original test dataset
artifacts$LOF_outlier <- factor(test_predictions, labels = c("Inlier", "Outlier"))


# Get the total number of observations
total_count <- nrow(test_data)

# Calculate the count of outliers and inliers
outlier_counts <- table(artifacts$LOF_outlier)

# Calculate the percentage of outliers and inliers
outliers_percentage <- (outlier_counts["Outlier"] / total_count) * 100
inliers_percentage <- (outlier_counts["Inlier"] / total_count) * 100

# Print the results
cat("Percentage of outliers:", outliers_percentage, "%\n")
cat("Percentage of inliers:", inliers_percentage, "%\n")

# Visualize the results using ggplot2
ggplot(test_results, aes(x = tsne1, y = tsne2, color = Prediction)) + 
  geom_point() +
  labs(title = "Artifact Outlier Detection using LOF, visualized with t-SNE", x = "t-SNE Dimension 1", y = "t-SNE Dimension 2") +
  scale_color_manual(values = c("red", "blue")) +  # Red for outliers, Blue for inliers
  theme_minimal()


## EXPORT DATA
# Add LOF results or original artifact dataframe
artifacts$LOF_outlier <- test_data$LOF_outlier

View(artifacts)

# Optionally, save the updated dataset to a CSV file
write.csv(artifacts, "artifacts_with_predictions_optimalfeatures.csv", row.names = FALSE)


# Optional: If you want to save the inliers as a separate CSV file
artifacts <- artifacts %>%
  filter(LOF_outlier == 1)  # Keep only outliers

View(artifacts)

# Export the outliers to a separate CSV file
write.csv(artifacts, "artifact_inliers_top20features.csv", row.names = FALSE)
