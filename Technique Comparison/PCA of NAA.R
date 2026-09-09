#----------------------------------------------------------
# Application of PCA to NAA data 
#----------------------------------------------------------

#install packages
if (!require(dbscan)) install.packages("dbscan", dependencies = TRUE)
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)
if (!require(dplyr)) install.packages("dplyr", dependencies = TRUE)
if (!require(ggrepel)) install.packages("ggrepel", dependencies = TRUE)
if (!require(compositions)) install.packages("compositions", dependencies = TRUE)
if (!require(zCompositions)) install.packages("zCompositions", dependencies = TRUE)

# Load necessary libraries
library(dbscan)  # For LOF
library(ggplot2)  # For plotting
library(dplyr) # For grouping and data manipulation
library(readxl) 
library(cowplot)
library(ggrepel) # For plotting
library(compositions)
library(zCompositions)

#----------------------------------------------------------
# Load Data and pre-processing steps
#----------------------------------------------------------
naa <- read.csv("Technique Comparison/INAA_comparison_dataset.csv")

colnames(naa)

# Remove non numeric data
numeric_naa <- naa[, sapply(naa, is.numeric)]


# Multiplicative zero replacement
naa_replaced <- cmultRepl(
  numeric_naa,
  label = 0,
  method = "CZM"
)

View(naa_replaced)

# CLR Transformation (instead of log10)
clr_naa <- clr(acomp(naa_replaced))

clr_naa <- as.data.frame(clr_naa)

colnames(clr_naa) <- colnames(numeric_naa)

head(clr_naa)

#----------------------------------------------------------
# Apply PCA
#----------------------------------------------------------
# Apply PCA to reduce dimensionality
pca_result_naa <- prcomp(clr_naa, scale. = TRUE)  # Exclude 'group' column
pca_data_naa <- as.data.frame(pca_result_naa$x)

# Extract loadings
loadings_naa <- as.data.frame(pca_result_naa$rotation[, 1:2])  # PC1 & PC2 loadings
loadings_naa$element <- rownames(loadings_naa)

#plot the first two PC of original data
pca_data_naa <- as.data.frame(pca_result_naa$x)

# % variance explained
pca_var <- pca_result_naa$sdev^2
pca_var_perc <- round(100 * pca_var / sum(pca_var), 1)

pc1_lab <- paste0("PC1 (", pca_var_perc[1], "%)")
pc2_lab <- paste0("PC2 (", pca_var_perc[2], "%)")

#----------------------------------------------------------
# Rescale Loadings for Plotting
#----------------------------------------------------------
loadings_naa <- loadings_naa %>%
  mutate(
    magnitude = sqrt(
      PC1^2 + PC2^2
    )
  )

loading_threshold_naa <- 0.18

loadings_strong_naa <- loadings_naa %>%
  filter(
    magnitude >= loading_threshold_naa
  )

# Determine size of PCA score space
score_max_naa <- max(
  abs(
    c(pca_data_naa$PC1,
      pca_data_naa$PC2)))


# Determine size of loading space
loading_max_naa <- max(
  abs(
    c(loadings_strong_naa$PC1,
      loadings_strong_naa$PC2)))

# Scale loading vectors to fit PCA score space
scale_factor_naa <- (
  score_max_naa /
    loading_max_naa) * 0.60


loadings_strong_naa <- loadings_strong_naa %>%
  mutate(
    PC1_plot = PC1 * scale_factor_naa,
    PC2_plot = PC2 * scale_factor_naa)


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


#----------------------------------------------------------
# Combined PCA Biplot
#----------------------------------------------------------

p_biplot_naa <- ggplot() +
  
  # PCA scores
  geom_point(
    data = pca_data_naa,
    aes(
      x = PC1,
      y = PC2,
      color = naa$Group
    ),
    size = 1.5,
    alpha = 0.9
  ) +
  
  # 90% confidence ellipses
  stat_ellipse(
    data = pca_data_naa,
    aes(
      x = PC1,
      y = PC2,
      group = naa$Group,
      color = naa$Group
    ),
    type = "t",
    level = 0.90
  ) +
  
  # Loading arrows
  geom_segment(
    data = loadings_strong_naa,
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
    data = loadings_strong_naa,
    aes(
      x = PC1_plot * 1.05,
      y = PC2_plot * 0.90,
      label = element
    ),
    size = 3.5,
    color = "black"
  ) +
  
  labs(
    title = "",
    x = pc1_lab,
    y = pc2_lab,
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

## Show both plots separately
p_scores_naa
p_loadings_naa
p_biplot_naa




