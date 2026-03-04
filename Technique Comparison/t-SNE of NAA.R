# Install the Rtsne package (if you haven't already)
install.packages("Rtsne")

# Load the library
library(Rtsne)
library(readxl) 

# Load the geochemical dataset
naa <- read.csv("Technique Comparison/INAA_comparison_dataset.csv")
View(naa) 

#remove non numeric data before running PCA
numeric_naa <- naa[, sapply(naa, is.numeric)]

# Scale the numeric columns
naa_scaled <- scale(numeric_naa)

# Apply t-SNE to the scaled data (e.g., reducing to 2 dimensions)
tsne_results_naa <- Rtsne(naa_scaled, dims = 3, perplexity = 15, pca = T, check_duplicates = F)

# Extract the t-SNE results (coordinates in 2D)
tsne_data_naa <- tsne_results_naa$Y

# Assuming you have a 'location' column or outlier info in your data
# Create a data frame for the results
tsne_naa <- data.frame(tsne_data_naa, Location = naa$Group)
print(tsne_naa)

# Plot the results using ggplot2
library(ggplot2)
library(viridis)

ggplot(tsne_naa, aes(x = X1, y=X2, color= Location)) +
  geom_point(size = 2) +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "",
       x = "t-SNE Dimension 1",
       y = "t-SNE Dimension 2") +
  theme_grey()
