#----------------------------------------------------------
# Application of PCA to LAICPMS data 
#----------------------------------------------------------

#install packages
if (!require(dbscan)) install.packages("dbscan", dependencies = TRUE)
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)
if (!require(dplyr)) install.packages("dplyr", dependencies = TRUE)

# Load necessary libraries
library(dbscan)  # For LOF
library(ggplot2)  # For plotting
library(dplyr) # For grouping and data manipulation
library(readxl) 
library(compositions)
library(zCompositions)


#----------------------------------------------------------
# Load Data
#----------------------------------------------------------
laicpms <- read.csv("Technique Comparison/LAICPMS_comparison_dataset.csv")

colnames(laicpms)

#remove non numeric data before running PCA
numeric_laicpms <- laicpms[, sapply(laicpms, is.numeric)]


# Multiplicative zero replacement
laicpms_replaced <- cmultRepl(
  numeric_laicpms,
  label = 0,
  method = "CZM"
)

# CLR Transformation (instead of log10)
clr_laicpms <- clr(acomp(laicpms_replaced))

clr_laicpms <- as.data.frame(clr_laicpms)

colnames(clr_laicpms) <- colnames(numeric_laicpms)

#----------------------------------------------------------
# Apply PCA
#----------------------------------------------------------
# Apply PCA to reduce dimensionality
pca_result_laicpms <- prcomp(clr_laicpms, scale. = TRUE)  # Exclude 'group' column
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

#--------------------------------
# Rescale loadings (for plotting)
#---------------------------------
loadings_laicpms <- loadings_laicpms %>%
  mutate(
    magnitude = sqrt(PC1^2 + PC2^2)
  )

# Select strong loadings for plotting
loading_threshold <- 0.18

loadings_strong_laicpms <- loadings_laicpms %>%
  filter(magnitude >= loading_threshold)

# Rescale loadings for plotting 

# Determine the size of the PCA score space
score_max <- max(
  abs(c(
    pca_data_laicpms$PC1,
    pca_data_laicpms$PC2
  ))
)

# Determine the size of the loading space
loading_max <- max(
  abs(c(
    loadings_strong_laicpms$PC1,
    loadings_strong_laicpms$PC2
  ))
)

# Scaling factor to place arrows inside score space
scale_factor <- (score_max / loading_max) * 0.60

loadings_strong_laicpms <- loadings_strong_laicpms %>%
  mutate(
    PC1_plot = PC1 * scale_factor,
    PC2_plot = PC2 * scale_factor
  )


#----------------------------------------------------------
# Plot PCA
#----------------------------------------------------------
#scores 
p_scores_laicpms <- ggplot(pca_data_laicpms, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = laicpms$Group), size = 1) +
  stat_ellipse(aes(group = laicpms$Group, color = laicpms$Group), type = "t", level = 0.90) +
  labs(
    title = "",
    x = pc1_lab_2,
    y = pc2_lab_2
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

#----------------------------------------------------------
# Combined PCA Biplot
#----------------------------------------------------------

p_biplot_laicpms <- ggplot() +
  
  # PCA scores
  geom_point(
    data = pca_data_laicpms,
    aes(
      x = PC1,
      y = PC2,
      color = laicpms$Group
    ),
    size = 1.5,
    alpha = 0.9
  ) +
  
  # 90% confidence ellipses
  stat_ellipse(
    data = pca_data_laicpms,
    aes(
      x = PC1,
      y = PC2,
      group = laicpms$Group,
      color = laicpms$Group
    ),
    type = "t",
    level = 0.90
  ) +
  
  # Loading arrows
  geom_segment(
    data = loadings_strong_laicpms,
    aes(
      x = 0,
      y = 0,
      xend = PC1_plot,
      yend = PC2_plot
    ),
    arrow = arrow(
      length = unit(0.15, "cm")
    ),
    linewidth = 0.6,
    color = "black"
  ) +
  
  # Loading labels
  geom_text(
    data = loadings_strong_laicpms,
    aes(
      x = PC1_plot * 1.15,
      y = PC2_plot * 1.10,
      label = element
    ),
    size = 3.5,
    color = "black"
  ) +
  
  labs(
    title = "",
    x = pc1_lab_2,
    y = pc2_lab_2,
    color = "Sample Group"
  ) +
  
  scale_color_manual(
    values = c(
      "red",
      "blue",
      "orange",
      "purple",
      "darkgreen",
      "lightblue"
    )
  ) +
  
  theme_grey()


# Display
p_biplot_laicpms

## Show both plots separately
p_scores_laicpms
p_loadings_laicpms 
p_biplot_laicpms
