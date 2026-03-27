#----------------------------------------------------------
# Application of PCA to LAICPMS data 
#----------------------------------------------------------

#install packages if not already installed 
if (!require(dbscan)) install.packages("dbscan", dependencies = TRUE)
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)
if (!require(dplyr)) install.packages("dplyr", dependencies = TRUE)

# Load necessary libraries
library(dbscan)  # For LOF
library(ggplot2)  # For plotting
library(dplyr) # For grouping and data manipulation
library(readxl) 

#----------------------------------------------------------
# Load Data
#----------------------------------------------------------
laicpms <- read.csv("Technique Comparison/LAICPMS_comparison_dataset.csv")

colnames(laicpms)
#remove non numeric data before running PCA
numeric_laicpms <- laicpms[, sapply(laicpms, is.numeric)]


# Apply log transformation 
log_numeric_laicpms <- log1p(numeric_laicpms)

#----------------------------------------------------------
# Apply PCA
#----------------------------------------------------------
# Apply PCA to reduce dimensionality
pca_result_laicpms <- prcomp(log_numeric_laicpms, scale. = TRUE)  # Exclude 'group' column
pca_data_laicpms <- as.data.frame(pca_result_laicpms$x)


# Extract loadings (rotation)
loadings_laicpms <- as.data.frame(pca_result_laicpms$rotation[, 1:2])  # PC1 & PC2 loadings
loadings_laicpms$element <- rownames(loadings_laicpms)

#plot the first two PC of original data
pca_data_laicpms <- as.data.frame(pca_result_laicpms$x)

# % variance explained
pca_var_2 <- pca_result_laicpms$sdev^2
pca_var_perc_2 <- round(100 * pca_var_2 / sum(pca_var_2), 1)

pc1_lab_2 <- paste0("PC1 (", pca_var_perc_2[1], "%)")
pc2_lab_2 <- paste0("PC2 (", pca_var_perc_2[2], "%)")

#rescale loadings (for plotting)
scale_factor_2 <- max(abs(pca_data_laicpms$PC1),
                      abs(pca_data_laicpms$PC2)) * 0.8

loadings_laicpms_scaled <- loadings_laicpms
loadings_laicpms_scaled$PC1 <- loadings_laicpms$PC1 * scale_factor_2
loadings_laicpms_scaled$PC2 <- loadings_laicpms$PC2 * scale_factor_2

loadings_strong_2 <- loadings_laicpms_scaled %>%
  filter(abs(PC1) > 0.25 * scale_factor_2 |
           abs(PC2) > 0.25 * scale_factor_2)


#----------------------------------------------------------
# Plot PCA
#----------------------------------------------------------
#scpres 
p_scores_laicpms <- ggplot(pca_data_laicpms, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = laicpms$Group), size = 1) +
  stat_ellipse(aes(group = laicpms$Group, color = laicpms$Group), type = "t", level = 0.90) +
  labs(
    title = "",
    x = pc1_lab,
    y = pc2_lab
  ) +
  scale_color_manual(
    name = "Sample Group",
    values = c("red", "blue", "orange", "purple", "darkgreen", "lightblue")
  ) +
  theme_grey()


# loadings plot
p_loadings_laicpms <- ggplot(loadings_laicpms, aes(x = PC1, y = PC2)) +
  geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.15, "cm")),
               color = "black") +
  geom_text(aes(label = element), vjust = 1.5, size = 3) +
  labs(
    title = "",
    x = "PC1",
    y = "PC2"
  ) +
  theme_grey()

#scores and loadings
p_biplot_laicpms <- ggplot() +
  
  # PCA scores
  geom_point(
    data = pca_data_laicpms,
    aes(x = PC1, y = PC2, color = laicpms$Group),
    size = 2, alpha = 0.9
  ) +
  
  # Loading arrows
  geom_segment(
    data = loadings_strong_2,
    aes(x = 0, y = 0, xend = PC1, yend = PC2),
    arrow = arrow(length = unit(0.1, "cm")),
    linewidth = 0.5,
    color = "black"
  ) +
  
  # Loading labels
  geom_text(
    data = loadings_strong_2,
    aes(x = PC1, y = PC2, label = element),
    size = 3, vjust = 1.2
  )+
  
  
  labs(
    title = "",
    x = pc1_lab_2,
    y = pc2_lab_2,
    color = "Sample Group"
  ) +
  
  scale_color_manual(values = c(
    "red", "blue", "orange", "purple", "darkgreen", "lightblue"
  )) +
  
  theme_grey()

## Show both plots separately
p_scores_laicpms
p_loadings_laicpms 
p_biplot_laicpms
