#install packages if not already installed 
if (!require(dbscan)) install.packages("dbscan", dependencies = TRUE)
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)
if (!require(dplyr)) install.packages("dplyr", dependencies = TRUE)


# Load necessary libraries
library(dbscan)  # For LOF
library(ggplot2)  # For plotting
library(dplyr) # For grouping and data manipulation
library(readxl)
library(Rtsne)

###PCA
# Load the geochemical dataset
optimalfeatures_artifacts <- read.csv("LAICPMS Data/artifacts_optimal_features.csv")

#remove non numeric data before running PCA
numeric_artifacts <- optimalfeatures_artifacts[, sapply(optimalfeatures_artifacts, is.numeric)]

print(numeric_artifacts )

## PCA of raw data, no outliers removed 
# Apply PCA to reduce dimensionality
pca_result_artifact <- prcomp(numeric_artifacts , scale. = TRUE)  # Exclude 'group' column
pca_data_artifact <- as.data.frame(pca_result_artifact$x)


#plot the first two PC of original data
pca_data_artifact <- as.data.frame(pca_result_artifact$x)
ggplot(pca_data_artifact, aes(x = PC1, y = PC2)) +
  geom_point(alpha = 1, aes(color = optimalfeatures_artifacts$Group)) +
  labs(title = "PCA", x = "PC1", y = "PC2", color = "Sites") +
  scale_color_manual(values = c(
    "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf", "#f07b47", "#ffb6c1", "#42f48b", "#e0d500", "#756e56", "#dd5f9b", 
    "#62a9d3", "#ff8b8b", "#c9ffcc", "#a3ffb3", "#1f6e66", "#f2993c", "#6a2d92"))


##t-SNE
# Select only numeric columns
print(numeric_artiafcts)

# Scale the numeric columns
numeric_artiafcts_scaled <- scale(numeric_artifacts)

nrow(numeric_artiafcts_scaled)

# Apply t-SNE to the scaled data (e.g., reducing to 2 dimensions)
tsne_artifacts <- Rtsne(numeric_artiafcts_scaled, dims = 3, perplexity = 5, pca = T, check_duplicates = F)

# Extract the t-SNE results (coordinates in 2D)
tsne_data_artifacts <- tsne_artifacts$Y

# Assuming you have a 'location' column or outlier info in your data
# Create a data frame for the results
tsne_artifacts <- data.frame(tsne_data_artifacts, Location = optimalfeatures_artifacts$Group)
print(tsne_artifacts)

# Plot the results using ggplot2
library(ggplot2)
library(viridis)

ggplot(tsne_artifacts, aes(x = X1, y=X2, color= Location)) +
  geom_point(size = 2) +
  scale_color_viridis(discrete = TRUE, option = "c") +
  labs(title = "t-SNE Visualization of artifact Data",
       caption = "Optimal Features. Perplexity set to 5",
       x = "t-SNE Dimension 1",
       y = "t-SNE Dimension 2",
       color = "Sites") +
  theme_grey()
