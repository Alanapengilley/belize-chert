#---------------------------------------------------------
# Results of comparing LDA and RF applications to NAA and LAICPMS
#----------------------------------------------------------
# Install patchwork if needed
install.packages("patchwork")

# Load libraries
library(ggplot2)
library(dplyr)
library(patchwork)

# Your data
data <- data.frame(
  Group = c("NAA", "LA-ICP-MS", "NAA", "LA-ICP-MS"),
  Method = c("RF", "RF", "LDA", "LDA"),
  Accuracy = c(80, 70, 70, 60),
  F1 = c(83.7, 76.7, 66.0, 46.7)
) # insert results from LDA and RF into this table


##------------------------------------------
# Plot accuracy
#-------------------------------------------
# Keep only Accuracy
accuracy_data <- data %>%
  dplyr::select(Group, Method, Accuracy)

# Custom colors
colors <- c("NAA" = "#FF5733", "LA-ICP-MS" = "orange")

# Vertical bar plot for accuracy
P1 <- ggplot(accuracy_data, aes(x = Method, y = Accuracy, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.5, alpha = 0.7) +
  geom_hline(yintercept = 60, linetype = "dashed", color = "black", linewidth = 0.4) +
  scale_fill_manual(values = colors) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(
    title = "Predictive Accuracy",
    x = "",
    y = "Accuracy (%)",
    fill = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(face = "bold"),
    legend.position = "top",
    panel.grid.major.x = element_blank()
  )


##------------------------------------------
# Plot Weighted F1
#-------------------------------------------
# Keep only Accuracy
F1_data <- data %>%
  dplyr::select(Group, Method, F1)

# Vertical bar plot
P2 <- ggplot(F1_data, aes(x = Method, y = F1, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.5, alpha = 0.7) +
  geom_hline(yintercept = 60, linetype = "dashed", color = "black", linewidth = 0.4) +
  scale_fill_manual(values = colors) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(
    title = "Weighted F1 Scores",
    x = "",
    y = "Weighted F1 (%)",
    fill = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(face = "bold"),
    legend.position = "top",
    panel.grid.major.x = element_blank()
  )


# Combine plots side by side
P1 + P2
