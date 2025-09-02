# Load necessary libraries
library(MASS)
library(ggplot2)
library(dplyr)
library(lattice)
library(caret)


inaa <- read.csv("Technique Comparison/INAA_comparison_dataset.csv") 

# Extract ANID column before it's removed. for final ploting purposes 
ANID_column <- inaa$ANID

# Remove the ANID column from the dataset
inaa <- inaa[, !(names(inaa) %in% c("ANID"))]

# Apply log10 transformation (+1 to avoid issues with zero)
inaa[ , sapply(inaa, is.numeric)] <- log10(inaa[ , sapply(inaa, is.numeric)] + 1)

#split data into training (80%) and test set (20%)
set.seed(2025)

training.samples_inaa <- inaa$Group %>%
  createDataPartition(p = 0.8, list = FALSE)

train.data_inaa <- inaa[training.samples_inaa, ]
test.data_inaa <- inaa[-training.samples_inaa, ]

ANID_train <- ANID_column[training.samples_inaa]
ANID_test <- ANID_column[-training.samples_inaa]

# Normalize the data 
# Estimate preprocessing parameters
preproc.param_inaa <- train.data_inaa %>% 
  preProcess(method = c("center", "scale"))

# Transform the data using the estimated parameters
train.transformed_inaa <- predict(preproc.param_inaa, train.data_inaa)
test.transformed_inaa <- predict(preproc.param_inaa, test.data_inaa)

# Check the structure of the transformed data to ensure it looks good
str(train.transformed_inaa)
str(test.transformed_inaa)

#Perform LDA
lda_inaa <- lda(Group ~ ., data = train.transformed_inaa)

# Make predictions
test.prediction_inaa <- predict(lda_inaa, test.transformed_inaa)

# Model accuracy
accuracy_inaa <- mean(test.prediction_inaa$class == test.transformed_inaa$Group)
print(paste("Model accuracy: ", accuracy_inaa))

# Predicted classes
head(test.prediction_inaa$class, 6)
# Predicted probabilities of class membership.
head(test.prediction_inaa$posterior, 6) 
# Linear discriminants
head(test.prediction_inaa$x, 3) 


lda.data_inaa <- cbind(train.transformed_inaa, predict(lda_inaa)$x)

ggplot(lda.data_inaa, aes(LD1, LD2)) +
  geom_point(alpha = 1, aes(color = Group)) +
  stat_ellipse(aes(color = Group)) +
  labs(title = "LDA of INAA Data", x = "LDA Component 1", y = "LDA Component 2", color = "Source Groups") +
  scale_color_manual(values = c("red", "blue", "green", "yellow", "orange", "purple", "pink", "brown", "gray", 
                                "black", "white", "cyan", "magenta", "lightblue", "darkgreen", "gold", 
                                "beige", "salmon", "peachpuff", "#E6E6FA", "darkblue", "turquoise")) +  # Hex code for lavender and dark blue
  annotate(
    "text", 
    x = Inf, y = -Inf, label = paste("Model Accuracy:", round(accuracy_inaa, 10)), 
    hjust = 1.1, vjust = -0.5, size = 4, fontface = "italic", color = "black"
  )




###Confusion Matrix
#Ensure predicted values and actual labels are factors 
predicted <- factor(test.prediction_inaa$class)
actual <- factor(test.transformed_inaa$Group)

#Match levels. Ensure they have the same levels. 
common_levels <- union(levels(predicted), levels(actual))

predicted <- factor(predicted, levels = common_levels)
actual <- factor(actual, levels = common_levels)

#Run Confusion Matrix 
conf_mat <- confusionMatrix(predicted, actual)
print(conf_mat)


#Plot training and test predictions 
test_plot_data <- test.transformed_inaa %>%
  mutate(
    LD1 = test.prediction_inaa$x[, 1],
    LD2 = test.prediction_inaa$x[, 2],
    Source = "Test",
    PredictedGroup = test.prediction_inaa$class,
    Correct = ifelse(PredictedGroup == Group, "Correct", "Incorrect"),
    ANID = ANID_test
  )

train_plot_data <- lda.data_inaa %>%
  mutate(
    Source = "Train",
    PredictedGroup = Group,
    Correct = "Correct",
    ANID = ANID_train
  )

plot_data <- bind_rows(train_plot_data, test_plot_data)
View(plot_data)

library(ggrepel)

#plot with all test and train, misclassified test labels and colored. 
ggplot(plot_data, aes(x = LD1, y = LD2)) +
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
    data = filter(plot_data, Correct == "Incorrect"),
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
    title = "INAA LDA Training and Test Data with Misclassifications",
    x = "LDA Component 1",
    y = "LDA Component 2",
    color = "Source group",
    shape = "Data Source",
    fill = "Prediction"
  ) +
  theme_minimal()



