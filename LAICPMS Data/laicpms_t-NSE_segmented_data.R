# Install the Rtsne package (if you haven't already)
install.packages("Rtsne")

# Load the library
library(Rtsne)
library(readxl)


# Load the laicpms dataset with only select samples for data comparison 
df_laicpms_segmented <- read_excel("LAICPMS DATA/laicpms_data_segmented_comparison.xlsx")
View(df_laicpms_segmented)


# Select only numeric columns
df_laicpms_segmented_numeric <- df_laicpms_segmented[sapply(df_laicpms_segmented, is.numeric)]

# Scale the numeric columns
df_laicpms_scaled <- scale(df_laicpms_segmented_numeric)


# Apply t-SNE to the scaled data (e.g., reducing to 2 dimensions)
tsne_results_laicpms_segmented <- Rtsne(df_laicpms_scaled, dims = 2, perplexity = 10, pca = T, check_duplicates = F)

# Extract the t-SNE results (coordinates in 2D)
tsne_data_laicpms <- tsne_results_laicpms_segmented$Y

# Assuming you have a 'location' column or outlier info in your data
# Create a data frame for the results
tsne_data_laicpms <- data.frame(tsne_data_laicpms, Location = df_laicpms_segmented $Group)
print(tsne_data_laicpms )

# Plot the results using ggplot2
library(ggplot2)
library(viridis)

ggplot(tsne_data_laicpms , aes(x = X1, y=X2, color= Location)) +
  geom_point(size = 2) +
  scale_color_viridis(discrete = TRUE, option = "C") +
  labs(title = "t-SNE Visualization of LAICPMS Data",
       caption = "Perplexity set to 15",
       x = "t-SNE Dimension 1",
       y = "t-SNE Dimension 2") +
  theme_minimal()

