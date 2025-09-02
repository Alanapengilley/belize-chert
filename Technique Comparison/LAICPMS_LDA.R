# Load necessary libraries
library(MASS)
library(ggplot2)
library(dplyr)
library(lattice)
library(caret)

#load in dataset
laicpms <- read.csv("Technique Comparison/LAICPMS_comparison_dataset.csv")
View(laicpms)

# Extract ANID column before it's removed. for final ploting purposes 
ANID_column_2 <- laicpms$ANID

# Remove the ANID column from the dataset
laicpms <- laicpms[, !(names(laicpms) %in% c("ANID"))]

# Apply log10 transformation (+1 to avoid issues with zero)
laicpms[ , sapply(laicpms, is.numeric)] <- log10(laicpms[ , sapply(laicpms, is.numeric)] + 1)

#split data into training (80%) and test set (20%)
set.seed(2025)

training.samples_laicpms <- laicpms$Group %>%
  createDataPartition(p = 0.8, list = FALSE)

train.data_laicpms <- laicpms[training.samples_laicpms, ]
test.data_laicpms <- laicpms[-training.samples_laicpms, ]

ANID_train_2 <- ANID_column_2[training.samples_laicpms]
ANID_test_2 <- ANID_column_2[-training.samples_laicpms]

# Normalize the data 
# Estimate preprocessing parameters
preproc.param_laicpms <- train.data_laicpms %>% 
  preProcess(method = c("center", "scale"))

# Transform the data using the estimated parameters
train.transformed_laicpms <- predict(preproc.param_laicpms, train.data_laicpms)
test.transformed_laicpms <- predict(preproc.param_laicpms, test.data_laicpms)

train.transformed_laicpms$Group <- train.data_laicpms$Group
test.transformed_laicpms$Group <- test.data_laicpms$Group

train.transformed_laicpms$ANID <- ANID_train_2
test.transformed_laicpms$ANID <- ANID_test_2

# Check the structure of the transformed data to ensure it looks good
str(train.transformed_laicpms)
str(test.transformed_laicpms)

#Perform LDA
lda_laicpms <- lda(Group ~ ., data = train.transformed_laicpms)

# Make predictions
test.prediction_laicpms <- predict(lda_laicpms, test.transformed_laicpms)

# Model accuracy
accuracy_laicpms <- mean(test.prediction_laicpms$class == test.transformed_laicpms$Group)
print(paste("Model accuracy: ", accuracy_laicpms))

# Predicted classes
head(test.prediction_laicpms$class, 6)
# Predicted probabilities of class membership.
head(test.prediction_laicpms$posterior, 6) 
# Linear discriminants
head(test.prediction_laicpms$x, 3) 


lda.data_laicpms <- cbind(train.transformed_laicpms, predict(lda_laicpms)$x)

ggplot(lda.data_laicpms, aes(LD1, LD2)) +
  geom_point(alpha = 1, aes(color = Group)) +
  stat_ellipse(aes(color = Group)) +
  labs(title = "LDA of LAICPMS Data", x = "LDA Component 1", y = "LDA Component 2", color = "Source Groups") +
  scale_color_manual(values = c("red", "blue", "green", "yellow", "orange", "purple", "pink", "brown")) +  # Hex code for lavender and dark blue
  annotate(
    "text", 
    x = Inf, y = -Inf, label = paste("Model Accuracy:", round(accuracy_laicpms, 10)), 
    hjust = 1.1, vjust = -0.5, size = 4, fontface = "italic", color = "black"
  )




###Confusion Matrix
#Ensure predicted values and actual labels are factors 
predicted_laicpms <- factor(test.prediction_laicpms$class)
actual_laicpms <- factor(test.transformed_laicpms$Group)

#Match levels. Ensure they have the same levels. 
common_levels_laicpms <- union(levels(predicted_laicpms), levels(actual_laicpms))

predicted_laicpms <- factor(predicted_laicpms, levels = common_levels_laicpms)
actual_laicpms <- factor(actual_laicpms, levels = common_levels_laicpms)

#Run Confusion Matrix 
conf_mat_laicpms <- confusionMatrix(predicted_laicpms, actual_laicpms)
print(conf_mat_laicpms)


#Plot training and test predictions 
test_plot_data_laicpms <- test.transformed_laicpms %>%
  mutate(
    LD1 = test.prediction_laicpms$x[, 1],
    LD2 = test.prediction_laicpms$x[, 2],
    Source = "Test",
    PredictedGroup = test.prediction_laicpms$class,
    Correct = ifelse(PredictedGroup == Group, "Correct", "Incorrect"),
    ANID = ANID_test_2
  )

train_plot_data_laicpms <- lda.data_laicpms %>%
  mutate(
    Source = "Train",
    PredictedGroup = Group,
    Correct = "Correct",
    ANID = ANID_train_2
  )

plot_data_laicpms <- bind_rows(train_plot_data_laicpms, test_plot_data_laicpms)
View(plot_data_laicpms)


library(ggrepel)

#plot with all test and train, misclassified test labels and colored. 
ggplot(plot_data_laicpms, aes(x = LD1, y = LD2)) +
  geom_point(
    aes(
      color = PredictedGroup,
      shape = Source,
      fill = Correct
    ),
    size = 1.5,
    stroke = 1.2
  ) +
  # 90% confidence ellipses for each true group
  stat_ellipse(aes(group = Group, color = Group),
               level = 0.90, linetype = "solid", size = 0.4) +
  # Label only misclassified points
  geom_text_repel(
    data = filter(plot_data_laicpms, Correct == "Incorrect"),
    aes(label = ANID),
    size = 2,
    max.overlaps = 50,
    box.padding = 0.4,
    point.padding = 0.4,
    segment.color = "gray50"
  ) +
  scale_shape_manual(values = c("Train" = 21, "Test" = 24)) +  # Hollow shapes
  scale_fill_manual(values = c("Correct" = "white", "Incorrect" = "black")) +
  scale_color_manual(values = c(
    "red", "orange", "purple", "lightblue", "darkgreen", 
    "salmon", "darkblue", "turquoise"
  )) +
  labs(
    title = "LAIOCPMS LDA Training and Test Data with Misclassifications",
    x = "LDA Component 1",
    y = "LDA Component 2",
    color = "Source group",
    shape = "Data Source",
    fill = "Prediction"
  ) +
  theme_minimal()



