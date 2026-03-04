#install packages if not already installed 
if (!require(dbscan)) install.packages("dbscan", dependencies = TRUE)
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)
if (!require(dplyr)) install.packages("dplyr", dependencies = TRUE)
if (!require(ggrepel)) install.packages("ggrepel", dependencies = TRUE)


# Load necessary libraries
library(dbscan)  # For LOF
library(ggplot2)  # For plotting
library(dplyr) # For grouping and data manipulation
library(readxl) 
library(cowplot)
library(ggrepel) # For plotting


#----------------------------------------------------------
# Load Data
#----------------------------------------------------------
naa <- read.csv("Technique Comparison/INAA_comparison_dataset.csv")

colnames(naa)

#remove non numeric data before running PCA
numeric_naa <- naa[, sapply(naa, is.numeric)]

# Apply log transformation 
log_numeric_naa <- log1p(numeric_naa)

#----------------------------------------------------------
# Apply PCA
#----------------------------------------------------------
# Apply PCA to reduce dimensionality
pca_result_naa <- prcomp(log_numeric_naa, scale. = TRUE)  # Exclude 'group' column
pca_data_naa <- as.data.frame(pca_result_naa$x)

# Extract loadings (rotation)
loadings_naa <- as.data.frame(pca_result_naa$rotation[, 1:2])  # PC1 & PC2 loadings
loadings_naa$element <- rownames(loadings_naa)

#plot the first two PC of original data
pca_data_naa <- as.data.frame(pca_result_naa$x)

# % variance explained
pca_var <- pca_result_naa$sdev^2
pca_var_perc <- round(100 * pca_var / sum(pca_var), 1)

pc1_lab <- paste0("PC1 (", pca_var_perc[1], "%)")
pc2_lab <- paste0("PC2 (", pca_var_perc[2], "%)")

#rescale loadings (for plotting)
scale_factor <- max(abs(pca_data_naa$PC1), abs(pca_data_naa$PC2)) * 0.8

loadings_naa_scaled <- loadings_naa
loadings_naa_scaled$PC1 <- loadings_naa$PC1 * scale_factor
loadings_naa_scaled$PC2 <- loadings_naa$PC2 * scale_factor
loadings_strong <- loadings_naa_scaled %>%
  filter(abs(PC1) > 0.25*scale_factor | abs(PC2) > 0.25*scale_factor)

#----------------------------------------------------------
# Plot PCA
#----------------------------------------------------------

p_scores_naa <- ggplot(pca_data_naa, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = naa$Group), size = 1) +
  stat_ellipse(aes(group = naa$Group, color = naa$Group), type = "t", level = 0.90) +
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


## Standalone loadings plot
p_loadings_naa <- ggplot(loadings_naa, aes(x = PC1, y = PC2)) +
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
p_biplot_naa <- ggplot() +
  
  # PCA scores
  geom_point(
    data = pca_data_naa,
    aes(x = PC1, y = PC2, color = naa$Group),
    size = 1.5, alpha = 0.9
  ) +
  
  # Loading arrows
  geom_segment(
    data = loadings_strong,
    aes(x = 0, y = 0, xend = PC1 * 0.95, yend = PC2 * 0.95),
    arrow = arrow(length = unit(0.15, "cm")),
    linewidth = 0.6,
    color = "black"
  ) +
  
  # Floating loading labels
  geom_text_repel(
    data = loadings_strong,
    aes(x = PC1 * 1.08, y = PC2 * 1.08, label = element),
    size = 3.5,
    color = "black",
    box.padding = 0,
    point.padding = 0,
    min.segment.length = 0
  ) +
  
  labs(
    title = "",
    x = pc1_lab,
    y = pc2_lab,
    color = "Sample Group"
  ) +
  
  scale_color_manual(values = c(
    "red", "blue", "orange", "purple", "darkgreen", "lightblue"
  )) +
  
  theme_grey()

## Show both plots separately
p_scores_naa
p_loadings_naa
p_biplot_naa

