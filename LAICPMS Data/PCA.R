#install packages if not already installed 
if (!require(dbscan)) install.packages("dbscan", dependencies = TRUE)
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)
if (!require(dplyr)) install.packages("dplyr", dependencies = TRUE)



# Load necessary libraries
library(dbscan)  # For LOF
library(ggplot2)  # For plotting
library(dplyr) # For grouping and data manipulation
library(readxl) 


# Load the geochemical dataset
df <- read_excel("Geology LAICPMS.xlsx") 

View(df)

#remove non numeric data before running PCA
numeric_data <- df[, sapply(df, is.numeric)]

print(numeric_data)

## PCA of raw data, no outliers removed 
# Apply PCA to reduce dimensionality
pca_result <- prcomp(numeric_data, scale. = TRUE)  # Exclude 'group' column
pca_data <- as.data.frame(pca_result$x)


#plot the first two PC of original data
pca_data <- as.data.frame(pca_result$x)
ggplot(pca_data, aes(x = PC1, y = PC2)) +
  geom_point(alpha = 1, aes(color = df$Group)) +
  labs(title = "PCA", x = "PC1", y = "PC2") +
  scale_color_manual(values = c(
    "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf", "#f07b47", "#ffb6c1", "#42f48b", "#e0d500", "#756e56", "#dd5f9b", 
                                "#62a9d3", "#ff8b8b", "#c9ffcc", "#a3ffb3", "#1f6e66", "#f2993c", "#6a2d92"))





# Apply LOF to PCA-transformed data
lof_result <- lof(pca_data, minPts = 20)  # Using 10 as minPts, adjust based on your data
outliers <- which(lof_result > 1)

print(outliers)
                  
#Remove outliers from original data
cleaned_data <- data[-outliers, ]

str(cleaned_data)
dim(cleaned_data)

print(cleaned_data)

numeric_data_cleaned <- cleaned_data[, sapply(cleaned_data, is.numeric)]
str(numeric_data_cleaned)

if (nrow(numeric_data_cleaned) > 1 && ncol(numeric_data_cleaned) > 0) {
  pca_cleaned_result <- prcomp(numeric_data_cleaned, scale. = TRUE)
  summary(pca_cleaned_result)
} else {
  cat("Not enough data left for PCA after cleaning.\n")
}

# Re-run PCA on cleaned data (optional)
pca_cleaned_result <- prcomp(cleaned_data[, -which(names(cleaned_data) == "group")], scale. = TRUE)
summary(pca_cleaned_result)


#plot the first two PC of cleaned data 
pca_cleaned_data <- as.data.frame(pca_cleaned_result$x)
ggplot(pca_cleaned_data, aes(x = PC1, y = PC2)) +
  geom_point(alpha = 0.5, aes(color = cleaned_data$group)) +
  labs(title = "PCA After Removing Outliers", x = "PC1", y = "PC2")




























#############




# Function to apply LOF for each geological source location
detect_outliers <- function(group) {
  # Ensure the group is ungrouped and only contains data for outlier detection
  group <- ungroup(group)
  
  # Exclude 'location' column and use geochemical data for outlier detection
  X <- group %>%
    select(-Group)
  
  # Standardize the data (z-scaling)
  X_scaled <- scale(X)
  
  # Apply LOF (using dbscan's lof function)
  lof_result <- lof(X_scaled, minPts = 7)  # Adjust minPts as needed
  
  # Add the LOF result to the group (1 for outliers, 0 for inliers)
  group$outlier <- ifelse(lof_result > 1.5, 1, 0)  # Adjust threshold for outliers
  
  return(group)
}

# Apply LOF to each geological source location
outlier_results <- df %>%
  group_by(Group) %>%
  do({
    detect_outliers(.)  # Apply detect_outliers function to each group
  }) %>%
  ungroup() 



data_cleaned <- df[-outliers, ] 

pca_result <- prcomp(data_cleaned, scale. = TRUE) 

# Summary of PCA to see variance explained by each principal component
summary(pca_result)

# Plot the cumulative explained variance
explained_variance <- cumsum(pca_result$sdev^2) / sum(pca_result$sdev^2)
plot(explained_variance, type = "b", xlab = "Number of Principal Components", 
     ylab = "Cumulative Explained Variance", main = "Cumulative Explained Variance by PC")

# Plot the first two principal components
pca_data <- as.data.frame(pca_result$x)  # Extract PCA results for plotting
ggplot(pca_data, aes(x = PC1, y = PC2)) + 
  geom_point(alpha = 0.5) + 
  labs(title = "PCA: First vs Second Principal Component", x = "PC1", y = "PC2")






















####
# Example: Create a data frame (replace this with your actual data)
set.seed(123)

#remove non numeric data from df
df_numeric <- df[sapply(df, is.numeric)]

# Scale the numeric columns
df_scaled <- scale(df_numeric)


# 2. Perform PCA using prcomp() (Principal Component Analysis)
pca_result <- prcomp(df_scaled, center = TRUE, scale. = TRUE)

# 3. Get the PCA summary
summary(pca_result)

# 4. Create a data frame with PCA results for visualization
pca_df <- as.data.frame(pca_result$x)  # PCA scores (principal components)
pca_df$Group <- df$Group  # Add the group information back for coloring

# 5. Visualize the first two principal components (PC1 vs PC2) with ggplot
ggplot(pca_df, aes(x = PC1, y = PC2, color = Group)) +
  geom_point() +
  labs(title = "PCA: Grouped by Group",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal() +
  theme(legend.position = "right")
