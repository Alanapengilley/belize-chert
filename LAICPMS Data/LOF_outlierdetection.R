### Local outlier Factor Model (LOF) for identification of outliers in geological data 
#This script applies Local Outlier Factor model as a method to identified individual outliers within each source group in just the geological dataset. 
#A total of 27 outliers for identified. 

##WORKFLOW 

#Install necessary packages 
install.packages("dbscan")

# Load necessary libraries
library(ggplot2)
library(dbscan)
library(dplyr)
library(scale)
library(readxl)
library(Rtsne)

## LOAD AND PREPROCESS THE DATA 

# Load the geochemical dataset, with importance features selected 
selected_features <- read.csv("subset_data_with_selected_features_top20.csv")
View(selected_features)

# check for missing values across the dataset 
sum(is.na(selected_features))

## APPLY LOF WITHIN EACH GROUP
# LOF will be applied for each group separately. Need to loop over the groups and calculate the LOF scores for each group individually. 

# Define a function to apply LOF on each group
apply_lof <- function(group_data, minPts = 20) {
  # Get the number of data points in the current group
  n <- nrow(group_data)
  
  # Ensure that minPts is at least 2, but not greater than the number of points
  minPts <- min(20, n - 1)
  
  # Remove the non-numeric column for LOF
  X_group <- group_data %>% select(-Group, -ANID)
  
  # Ensure that the data is numeric (since LOF works on numeric data)
  X_group <- as.data.frame(lapply(X_group, as.numeric))
  
  # Compute LOF scores
  lof_scores <- lof(X_group, minPts = minPts)
  
  # Assign outliers (-1) and inliers (1) based on LOF scores
  group_data$LOF_outlier <- ifelse(lof_scores > 1.5, -1, 1)  # Adjust threshold as necessary
  return(group_data)
}

# Apply LOF for each group
df_grouped <- selected_features %>%
  group_by(Group) %>%               # Group by the 'Group' column
  do(apply_lof(.))                  # Apply the LOF function to each group (using 'do')



## APPLY t-SNE FOR DIMENSIONALITY REDUCTION 

# Ensure that the features are numeric
X_numeric <- df_grouped %>% select(-Group, -LOF_outlier)

# Apply t-SNE for dimensionality reduction to 2D
tsne_result <- Rtsne(X_numeric, dims = 2, check_duplicates = FALSE)

# Add t-SNE results to the dataframe
df_grouped$TSNE1 <- tsne_result$Y[, 1]
df_grouped$TSNE2 <- tsne_result$Y[, 2]



## VISUALIZATION 

# Load necessary library for color palettes
library(RColorBrewer)

# Check how many unique groups there are
num_groups <- length(unique(df_grouped$Group))

# Generate a color palette with enough colors
colors <- brewer.pal(min(num_groups, 12), "Set3")  # "Set3" palette can support up to 12 colors
if (num_groups > 12) {
  colors <- rainbow(num_groups)  # Use rainbow palette if there are more than 12 groups
}

#plot using ggplot
ggplot(df_grouped, aes(x = TSNE1, y = TSNE2, color = Group, shape = factor(LOF_outlier))) +
  geom_point(size = 2) +
  scale_shape_manual(values = c(16, 3)) +  # Different shapes for normal vs outliers
  scale_color_manual(values = colors) +    # Use the dynamic color palette
  labs(title = "t-SNE Plot with LOF Outliers Highlighted",
       x = "t-SNE Component 1", y = "t-SNE Component 2") +
  theme_minimal() +
  theme(legend.position = "top")



#SUMMARY OF RESULTS
# Show a sample of inliers with relevant columns
inliers_summary <- inliers %>% select(Group, TSNE1, TSNE2, LOF_outlier)
head(inliers_summary)

# Show a sample of outliers with relevant columns
outliers_summary <- outliers %>% select(Group, TSNE1, TSNE2, LOF_outlier)
head(outliers_summary)
View(outliers_summary)


## REMOVE OUTLIERS (LOF_outlier == -1)

# Filter the dataset to remove rows where LOF_outlier is -1 (outliers)
df_cleaned <- df_grouped %>%
  filter(LOF_outlier == 1)  # Only keep inliers (LOF_outlier == 1)

# View the cleaned dataset
View(df_cleaned)

# EXPORT THE CLEANED DATAFRAME

# Export the cleaned dataframe to a CSV file
write.csv(df_cleaned, "df_cleaned_no_outliers_top20features.csv", row.names = FALSE)

# Optional: If you want to save the outliers as a separate CSV file
df_outliers <- df_grouped %>%
  filter(LOF_outlier == -1)  # Keep only outliers

# Export the outliers to a separate CSV file
write.csv(df_outliers, "df_outliers_top20features.csv", row.names = FALSE)


#export inliers as seperate file 
