#----------------------------------------------------------
# Application of t-SNE to LAICPMS data 
#----------------------------------------------------------
# Install the Rtsne package
install.packages("Rtsne")

# Load the library
library(Rtsne)
library(readxl) 

#----------------------------------------------------------
# Load Data
#----------------------------------------------------------
laicpms <- read.csv("Technique Comparison/LAICPMS_comparison_dataset.csv")

#----------------------------------------------------------
# Pre-processing 
#----------------------------------------------------------
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

# Standardize CLR-transformed variables 
clr_laicpms_scaled <- scale(
  clr_laicpms,
  center = TRUE,
  scale = TRUE
)

#----------------------------------------------------------
# Apply t-SNE
#----------------------------------------------------------
# Apply t-SNE to the scaled data (e.g., reducing to 2 dimensions)
tsne_results_laicpms <- Rtsne(clr_laicpms_scaled, dims = 3, perplexity = 15, pca = T, check_duplicates = F)

# Extract the t-SNE results (coordinates in 2D)
tsne_data_laicpms <- tsne_results_laicpms$Y

# Assuming you have a 'location' column or outlier info in your data
# Create a data frame for the results
tsne_laicpms <- data.frame(tsne_data_laicpms, Location = laicpms$Group)
print(tsne_laicpms)

#----------------------------------------------------------
# Plot t-SNE (using ggplot)
#----------------------------------------------------------
library(ggplot2)
library(viridis)

ggplot(tsne_laicpms, aes(x = X1, y=X2, color= Location)) +
  geom_point(size = 2) +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "",
       x = "t-SNE Dimension 1",
       y = "t-SNE Dimension 2") +
  theme_grey()
