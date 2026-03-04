# Install the Rtsne package (if you haven't already)
install.packages("Rtsne")

# Load the library
library(Rtsne)
library(readxl) 

# Load the geochemical dataset
laicpms <- read.csv("Technique Comparison/LAICPMS_comparison_dataset.csv")
View(laicpms)

#remove non numeric data before running PCA
numeric_laicpms <- laicpms[, sapply(laicpms, is.numeric)]

# Scale the numeric columns
laicpms_scaled <- scale(numeric_laicpms)


# Apply t-SNE to the scaled data (e.g., reducing to 2 dimensions)
tsne_results_laicpms <- Rtsne(laicpms_scaled, dims = 3, perplexity = 15, pca = T, check_duplicates = F)

# Extract the t-SNE results (coordinates in 2D)
tsne_data_laicpms <- tsne_results_laicpms$Y

# Assuming you have a 'location' column or outlier info in your data
# Create a data frame for the results
tsne_laicpms <- data.frame(tsne_data_laicpms, Location = laicpms$Group)
print(tsne_laicpms)

# Plot the results using ggplot2
library(ggplot2)
library(viridis)

ggplot(tsne_laicpms, aes(x = X1, y=X2, color= Location)) +
  geom_point(size = 2) +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "",
       x = "t-SNE Dimension 1",
       y = "t-SNE Dimension 2") +
  theme_grey()
