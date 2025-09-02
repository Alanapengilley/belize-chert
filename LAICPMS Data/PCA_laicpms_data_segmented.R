#PCA Analysis of segmented LAICPMS data for comparison with other datasets 

# Load necessary libraries
library(dbscan)  # For LOF
library(ggplot2)  # For plotting
library(dplyr) # For grouping and data manipulation
library(readxl)
library(FactoMineR)
library(factoextra)


# Load the laicpms dataset with only select samples for data comparison 
df_laicpms_subset <- read_excel("LAICPMS DATA/laicpms_data_segmented_comparison.xlsx")
View(df_laicpms_subset)

# Check variables in dataset 
colnames(df_laicpms_subset)

# Log10 transform
laicpms_subset_log10 <- df_laicpms_subset
laicpms_subset_log10[] <- lapply(laicpms_subset_log10, function(x) if(is.numeric(x)) log10(x) else x)

print(laicpms_subset_log10)

# Remove Th and Nb as negative, and potentially problematic data recovery 
data_laicpms_refined <- laicpms_subset_log10[, !(names(laicpms_subset_log10) %in% c("Nb", "Th"))]
print(data_laicpms_refined)

# Check for NA values in the cleaned data
any(is.na(data_laicpms_refined)) # false means there are none, true means there are still NA values 

# Remove rows with Inf values from the cleaned data
data_laicpms_clean <- data_laicpms_refined%>%
  filter_all(all_vars(!is.infinite(.)))

# Check the cleaned data
print(data_laicpms_clean)

# Remove non-numeric data before running PCA
numeric_data_laicpms_subset <- data_laicpms_clean[, sapply(data_laicpms_clean, is.numeric)]
print(numeric_data_laicpms_subset)

## PCA 
# Apply PCA to reduce dimensionality
laicpms_data_results <- prcomp(numeric_data_laicpms_subset, scale. = TRUE)  # Exclude 'group' column
laicpms_pca_data <- as.data.frame(laicpms_data_results$x)

print(laicpms_pca_data)

loadings <- data.frame(laicpms_data_results$rotation)
print(loadings)

# View summary of PCA results
summary(laicpms_data_results)

#graph of variables 
fviz_pca_var(laicpms_data_results, col.var = "black")


#biplot of individuals and variables 
fviz_pca_biplot(laicpms_data_results, label = "var", habillage = data_laicpms_clean$Group,
                addEllipses = FALSE, ellipse.level = 0.95, palette = "viridis")

#plot the first two PC of original data
ggplot(laicpms_pca_data, aes(x = PC1, y = PC2)) +
  geom_point(alpha = 1, aes(color = data_laicpms_clean$Group)) +
  labs(title = "PCA of LAICPMS Data", x = "PC1", y = "PC2", color = "Source Groups") +
  scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#bcbd22", "#1f6e66", "#6a2d92"))

