

# Load necessary libraries
library(dbscan)  # For LOF
library(ggplot2)  # For plotting
library(dplyr) # For grouping and data manipulation
library(readxl)
library("FactoMineR")
library("factoextra")


# Load the EDXRF Dataset 
df_edxrf <- read_excel("EDXRF/EDXRF.xlsx") 

View(df_edxrf)


#log10 transform
edxrf_log10 <- df_edxrf
edxrf_log10[] <- lapply(edxrf_log10, function(x) if(is.numeric(x)) log10(x) else x)
  
print(edxrf_log10)

#remove Th as negative, and potentially problematic data recovery 
data_edxrf <- subset(edxrf_log10, select = -Th)
  
#remove non numeric data before running PCA
numeric_data_edxrf <- data_edxrf[, sapply(data_edxrf, is.numeric)]
  
print(numeric_data_edxrf)

## PCA 
# Apply PCA to reduce dimensionality
edxrf_pca_result <- prcomp(numeric_data_edxrf, scale. = TRUE)  # Exclude 'group' column
edxrf_pca_data <- as.data.frame(pca_result$x)


loadings <- data.frame(edxrf_pca_result$rotation)

#graph of variables 
fviz_pca_var(edxrf_pca_result, col.var = "black")

#biplot of individuals and variables 
fviz_pca_biplot(edxrf_pca_result, label = "var", habillage=df_edxrf$Group,
                addEllipses=FALSE, ellipse.level=0.95, palette = "viridis")

#plot the first two PC of original data
edxrf_pca_data <- as.data.frame(edxrf_pca_result$x)
ggplot(edxrf_pca_data, aes(x = PC1, y = PC2)) +
  geom_point(alpha = 1, aes(color = df_edxrf$Group)) +
  labs(title = "PCA of EDXRF Data", x = "PC1", y = "PC2", color ="Source Groups") +
  scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#bcbd22",  "#1f6e66" , "#6a2d92"))


