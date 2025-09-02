#PEARSONS PRODUCT-MOMENT CORRELATION COEFFICIENT (r) 
#calculated for each element between the comparison datasets 

#LOAD IN NECESSARY PACKAGES 
library(ggplot2)
library(reshape2)

#LOAD IN DATASETS 
#LA-ICP-MS DATASET
df_laicpms_subset <- read_excel("LAICPMS DATA/laicpms_data_segmented_comparison.xlsx")

#ED-XRF DATASET 
df_edxrf <- read_excel("EDXRF/EDXRF.xlsx") 

#PREPROCESSING OF DATA 

# Check if they have the same column names
common_elements <- intersect(colnames(df_laicpms_subset), colnames(df_edxrf))

# Select only the common elements (geochemical elements) from both datasets
df_laicpms_common <- df_laicpms_subset[, common_elements]
df_edxrf_common <- df_edxrf[, common_elements]

# Ensure that both datasets are numeric (i.e., contain only numeric data)
df_laicpms_common <- df_laicpms_common %>% select_if(is.numeric)
df_edxrf_common <- df_edxrf_common %>% select_if(is.numeric)

#CALCULATE PEARSON'S COEFFICIENT 

# Calculate Pearson's correlation between the corresponding elements in both datasets
correlation_matrix <- cor(df_laicpms_common, df_edxrf_common, method = "pearson")

# Print the correlation matrix
print(correlation_matrix)

# Melt the correlation matrix for ggplot
cor_melted <- melt(correlation_matrix)

# Create a heatmap of the correlation matrix
ggplot(cor_melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  labs(title = "Pearson's Product-Moment Correlation Coefficient Matrix", x = "LAICPMS Elements", y = "EDXRF Elements")

#create table
correlation_df <- as.data.frame(as.table(correlation_matrix))

# Rename the columns for better clarity
colnames(correlation_df) <- c("LAICPMS", "EDXRF", "Correlation_Coefficient")

# Filter to keep only the rows where the element names match between LAICPMS and EDXRF
matching_elements_df <- correlation_df %>% filter(LAICPMS == EDXRF)

# Print the resulting correlation table
print(matching_elements_df)
