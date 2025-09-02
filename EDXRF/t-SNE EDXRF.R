# Install the Rtsne package (if you haven't already)
install.packages("Rtsne")

# Load the library
library(Rtsne)

# Select only numeric columns
df_edxrf_numeric <- df_edxrf[sapply(df_edxrf, is.numeric)]

# Scale the numeric columns
df_edxrf_scaled <- scale(df_edxrf_numeric)


# Apply t-SNE to the scaled data (e.g., reducing to 2 dimensions)
tsne_results_edxrf <- Rtsne(df_edxrf_scaled, dims = 2, perplexity = 15, pca = T, check_duplicates = F)

# Extract the t-SNE results (coordinates in 2D)
tsne_data_edxrf <- tsne_results_edxrf$Y

# Assuming you have a 'location' column or outlier info in your data
# Create a data frame for the results
tsne_df_edxrf <- data.frame(tsne_data_edxrf, Location = df_edxrf$Group)
print(tsne_df_edxrf)

# Plot the results using ggplot2
library(ggplot2)
library(viridis)

ggplot(tsne_df_edxrf, aes(x = X1, y=X2, color= Location)) +
  geom_point(size = 1) +
  scale_color_viridis(discrete = TRUE, option = "C") +
  labs(title = "t-SNE Visualization of EDXRF Data",
       caption = "Perplexity set to 15",
       x = "t-SNE Dimension 1",
       y = "t-SNE Dimension 2") +
  theme_grey()




###Perplexity test####
install.packages("gganimate")
library(gganimate)

install.packages("RColorBrewer")
library(RColorBrewer)

# Generate a color palette that has enough colors for the number of unique categories
num_colors <- length(unique(tsne_df$digit))  # Number of unique categories
colors <- brewer.pal(min(num_colors, 12), "Set3") 

# If there are more than 12 categories, you can repeat the palette
if (num_colors > 12) {
  colors <- rep(colors, length.out = num_colors)
}

perplexity_values <- c(5, 10, 15, 20, 25, 30)

tsne_results_list <- lapply(perplexity_values, function(perp) {
  tsne <- Rtsne(df_edxrf, dims = 2, perplexity = perp, verbose = TRUE, max_iter = 1500)
  data.frame(
    X = tsne$Y[, 1],
    Y = tsne$Y[, 2],
    digit = df_edxrf$Group,
    perplexity = perp
  )
})

tsne_df <- do.call(rbind, tsne_results_list)

plot <- ggplot(tsne_df, aes(x = X, y = Y, color = factor(digit))) +
  geom_point(size = 1.5) +
  scale_color_manual(values = colors) +
  labs(
    title = "t-SNE 2-Dimensional Digit Visualization",
    subtitle = "Perplexity: {closest_state}", # This will display the perplexity value
    x = "t-SNE Dimension 1",
    y = "t-SNE Dimension 2"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 16)
  ) +
  transition_states(perplexity, transition_length = 2, state_length = 1) +
  ease_aes("linear")

animate(
  plot,
  width = 800,
  height = 600,
  res = 100,
  nframes = 300,
  fps = 30,
  renderer = gifski_renderer(file = "tsne-2d-animated.gif")
)
