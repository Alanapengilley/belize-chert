# Install the Rtsne package (if you haven't already)
install.packages("Rtsne")

# Load the library
library(Rtsne)

# Select only numeric columns
df_numeric <- df[sapply(df, is.numeric)]

# Scale the numeric columns
df_scaled <- scale(df_numeric)


# Apply t-SNE to the scaled data (e.g., reducing to 2 dimensions)
tsne_results <- Rtsne(df_scaled, dims = 3, perplexity = 15, pca = T, check_duplicates = F)

# Extract the t-SNE results (coordinates in 2D)
tsne_data <- tsne_results$Y

# Assuming you have a 'location' column or outlier info in your data
# Create a data frame for the results
tsne_df <- data.frame(tsne_data, Location = df$Group)
print(tsne_df)

# Plot the results using ggplot2
library(ggplot2)
library(viridis)

ggplot(tsne_df, aes(x = X1, y=X2, color= Location)) +
  geom_point(size = 2) +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "t-SNE Visualization of Geochemical Data",
       x = "t-SNE Dimension 1",
       y = "t-SNE Dimension 2") +
  theme_grey()

