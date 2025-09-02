# Load necessary libraries
library(MASS)
library(ggplot2)
library(dplyr)
library(lattice)
library(caret)
library(readxl)

# Load the EDXRF Dataset 
df_laicpms_subset <- read_excel("LAICPMS DATA/laicpms_data_segmented_comparison.xlsx")


colnames(df_laicpms_subset)

# Remove the ANID column from the dataset
df_laicpms_subset$ANID <- NULL

#split data into training (80%) and test set (20%)
set.seed(2025)

training.samples_laicpms_subset <- df_laicpms_subset$Group %>%
  createDataPartition(p = 0.8, list = FALSE)

train.data_laicpms_subset <- df_laicpms_subset[training.samples_laicpms_subset, ]
test.data_laicpms_subset <- df_laicpms_subset[-training.samples_laicpms_subset, ]

# Normalize the data 
# Estimate preprocessing parameters
preproc.param_laicpms_subset <- train.data_laicpms_subset%>% 
  preProcess(method = c("center", "scale"))

# Transform the data using the estimated parameters
train.transformed_laicpms_subset <- predict(preproc.param_laicpms_subset, train.data_laicpms_subset)
test.transformed_laicpms_subset <- predict(preproc.param_laicpms_subset, test.data_laicpms_subset)

# Check the structure of the transformed data to ensure it looks good
str(train.transformed_laicpms_subset )
str(test.transformed_laicpms_subset)

#Perform LDA
lda_laicpms_subset <- lda(Group ~ ., data = train.transformed_laicpms_subset)

# Make predictions
test.prediction_laicpms_subset <- predict(lda_laicpms_subset, test.transformed_laicpms_subset)


# Model accuracy
accuracy_laicpms <- mean(test.prediction_laicpms_subset$class == test.transformed_laicpms_subset$Group)
print(paste("Model accuracy: ", accuracy_laicpms))

# Predicted classes
head(test.prediction_laicpms_subset$class, 6)
# Predicted probabilities of class membership.
head(test.prediction_laicpms_subset$posterior, 6) 
# Linear discriminants
head(test.prediction_laicpms_subset$x, 3) 

lda.data_laicpms_subset <- cbind(train.transformed_laicpms_subset, predict(lda_laicpms_subset)$x)

ggplot(lda.data_laicpms_subset, aes(LD1, LD2)) +
  geom_point(alpha = 1, aes(color = Group )) +
  stat_ellipse(aes(color = Group))+
  labs(title = "LDA of LAICPMS Data", x = "LDA Component 1", y = "LDA Component 2", color = "Source Groups") +
  scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#bcbd22", "#1f6e66", "#6a2d92")) +
  
  annotate(
    "text", 
    x = Inf, y = -Inf, label = paste("LDA Model Accuracy:", round(accuracy_laicpms, 2)), 
    hjust = 1.1, vjust = -0.5, size = 4, fontface = "italic", color = "black"
  )



