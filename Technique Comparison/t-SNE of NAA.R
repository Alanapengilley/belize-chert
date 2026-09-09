#----------------------------------------------------------
# Application of t-SNE to NAA data 
#----------------------------------------------------------
# Install the Rtsne package
install.packages("Rtsne")

# Load the library
library(Rtsne)
library(readxl) 

#----------------------------------------------------------
# Load Data
#----------------------------------------------------------
naa <- read.csv("Technique Comparison/INAA_comparison_dataset.csv")

#----------------------------------------------------------
# Pre-processing 
#----------------------------------------------------------
# Remove non numeric data before running PCA
numeric_naa <- naa[, sapply(naa, is.numeric)]


# Multiplicative zero replacement
naa_replaced <- cmultRepl(
  numeric_naa,
  label = 0,
  method = "CZM"
)

# CLR Transformation (instead of log10)
clr_naa <- clr(acomp(naa_replaced))

clr_naa <- as.data.frame(clr_naa)

colnames(clr_naa) <- colnames(numeric_naa)

# Standardize CLR-transformed variables 
clr_naa_scaled <- scale(
  clr_naa,
  center = TRUE,
  scale = TRUE
)

#----------------------------------------------------`````------
# Apply t-SNE
#----------------------------------------------------------
# Apply t-SNE to the scaled data (e.g., reducing to 2 dimensions)
tsne_results_naa <- Rtsne(clr_naa_scaled, dims = 3, perplexity = 15, pca = T, check_duplicates = F)

# Extract the t-SNE results (coordinates in 2D)
tsne_data_naa <- tsne_results_naa$Y

# Assuming you have a 'location' column or outlier info in your data
# Create a data frame for the results
tsne_naa <- data.frame(tsne_data_naa, Location = naa$Group)
print(tsne_naa)

#----------------------------------------------------------
# Plot t-SNE (using ggplot)
#----------------------------------------------------------
library(ggplot2)
library(viridis)

ggplot(tsne_naa, aes(x = X1, y=X2, color= Location)) +
  geom_point(size = 2) +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "",
       x = "t-SNE Dimension 1",
       y = "t-SNE Dimension 2") +
  theme_grey()
