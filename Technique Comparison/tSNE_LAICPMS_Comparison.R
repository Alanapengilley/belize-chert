#t-SNE of LAICPMS data for comparison

# Install the Rtsne package (if you haven't already)
install.packages("Rtsne")

# Load the library
library(Rtsne)
library(readxl)

# Load the laicpms dataset with only select samples for data comparison 
df_laicpms_subset <- read_excel("LAICPMS DATA/laicpms_data_segmented_comparison.xlsx")

# Select only numeric columns
df_numeric_laicpms_subset <- df_laicpms_subset[sapply(df_laicpms_subset, is.numeric)]

# Scale the numeric columns
df_scaled_laicpms_subset <- scale(df_numeric_laicpms_subset)

# Apply t-SNE to the scaled data (e.g., reducing to 2 dimensions)
tsne_results_laicpms_subset <- Rtsne(df_scaled_laicpms_subset, dims = 3, perplexioty = 15, pca = T, check_duplicates = F)

# Extract the t-SNE results (coordinates in 2D)
tsne_data_laipcms_subset <- tsne_results_laicpms_subset$Y

# Assuming you have a 'location' column or outlier info in your data
# Create a data frame for the results
tsne_df_laicpms_subset <- data.frame(tsne_data_laipcms_subset, Location = df_laicpms_subset$Group)
print(tsne_df_laicpms_subset)

# Plot the results using ggplot2
library(ggplot2)
library(viridis)

ggplot(tsne_df_laicpms_subset, aes(x = X1, y=X2, color= Location)) +
  geom_point(size = 1) +
  scale_color_viridis(discrete = TRUE, option = "c") +
  labs(title = "t-SNE Visualization of LAICPMS Data",
       caption = "Perplexity set to 15",
       x = "t-SNE Dimension 1",
       y = "t-SNE Dimension 2") +
  theme_grey()

ggplot(tsne_df_edxrf, aes(x = X1, y=X2, color= Location)) +
  geom_point(size = 1) +
  scale_color_viridis(discrete = TRUE, option = "C") +
  labs(title = "t-SNE Visualization of EDXRF Data",
       caption = "Perplexity set to 15",
       x = "t-SNE Dimension 1",
       y = "t-SNE Dimension 2") +
  theme_grey()




