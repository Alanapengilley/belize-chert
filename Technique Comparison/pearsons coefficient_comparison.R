#PEARSONS PRODUCT-MOMENT CORRELATION COEFFICIENT (r) 
#calculated for each element between the comparison datasets 

#install packages 
install.packages("writexl")

#LOAD IN NECESSARY PACKAGES 
library(ggplot2)
library(reshape2)
library(writexl)

#LOAD IN DATASETS 
#LA-ICP-MS DATASET
laicpms <- read.csv("Technique Comparison/LAICPMS_comparison_dataset.csv")

#INAA Dataset
inaa <- read.csv("Technique Comparison/INAA_comparison_dataset.csv")

#PREPROCESSING OF DATA 

# Check if they have the same column names
common_elements_1 <- intersect(colnames(laicpms), colnames(inaa))

# Select only the common elements (geochemical elements) from both datasets
laicpms_common <- laicpms[, common_elements_1]
inaa_common <- inaa[, common_elements_1]

# Ensure that both datasets are numeric (i.e., contain only numeric data)
df_laicpms_common_1 <- laicpms_common %>% select_if(is.numeric)
df_inaa_common_1 <- inaa_common %>% select_if(is.numeric)

#CALCULATE PEARSON'S COEFFICIENT 

# Calculate Pearson's correlation between the corresponding elements in both datasets
correlation_matrix_comparison <- cor(df_laicpms_common_1, df_inaa_common_1, method = "pearson")

# Print the correlation matrix
print(correlation_matrix_comparison)

# Melt the correlation matrix for ggplot
cor_melted_comparison <- melt(correlation_matrix_comparison)

# Create a heatmap of the correlation matrix
ggplot(cor_melted_comparison, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  labs(title = "Pearson's Product-Moment Correlation Coefficient Matrix", x = "LAICPMS Elements", y = "INAA Elements")

#create table
correlation_df_comparison <- as.data.frame(as.table(correlation_matrix_comparison))

# Rename the columns for better clarity
colnames(correlation_df_comparison) <- c("LAICPMS", "INAA", "Correlation_Coefficient")

# Filter to keep only the rows where the element names match between LAICPMS and EDXRF
matching_elements_df_comparison <- correlation_df_comparison %>% filter(LAICPMS == INAA)

# Print the resulting correlation table
print(matching_elements_df_comparison)

#export final table
write_xlsx(matching_elements_df_comparison, "Technique Comparison/Matching_Correlation_Table_LAICPMS_INAA.xlsx")
