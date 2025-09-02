# Load necessary libraries
library(MASS)
library(ggplot2)
library(dplyr)
library(lattice)
library(caret)
library(readxl)

# Load the EDXRF Dataset 
df_edxrf <- read_excel("EDXRF/EDXRF.xlsx") 

colnames(df_edxrf)

# Remove the ANID column from the dataset
df_edxrf$ANID <- NULL

#split data into training (80%) and test set (20%)
set.seed(2025)

training.samples_edxrf <- df_edxrf$Group %>%
  createDataPartition(p = 0.8, list = FALSE)

train.data_edxrf <- df_edxrf[training.samples_edxrf, ]
test.data_edxrf <- df_edxrf[-training.samples_edxrf, ]


# Normalize the data 
# Estimate preprocessing parameters
preproc.param_edxrf <- train.data_edxrf %>% 
  preProcess(method = c("center", "scale"))

# Transform the data using the estimated parameters
train.transformed_edxrf <- predict(preproc.param_edxrf, train.data_edxrf)
test.transformed_edxrf <- predict(preproc.param_edxrf, test.data_edxrf)

# Check the structure of the transformed data to ensure it looks good
str(train.transformed_edxrf)
str(test.transformed_edxrf)

#Perform LDA
lda_edxrf <- lda(Group ~ ., data = train.transformed_edxrf)

# Make predictions
test.prediction_edxrf <- lda_edxrf %>% predict(test.transformed_edxrf)

# Model accuracy

accuracy_edxrf <- mean(test.prediction_edxrf$class==test.transformed_edxrf$Group)
print(paste("Model accuracy: ", accuracy_edxrf))

# Predicted classes
head(test.prediction_edxrf$class, 6)
# Predicted probabilities of class membership.
head(test.prediction_edxrf$posterior, 6) 
# Linear discriminants
head(test.prediction_edxrf$x, 3) 

lda.data_edxrf <- cbind(train.transformed_edxrf, predict(lda_edxrf)$x)

ggplot(lda.data_edxrf, aes(LD1, LD2)) +
  geom_point(alpha = 1, aes(color = Group )) +
  stat_ellipse(aes(color = Group))+
  labs(title = "LDA of EDXRF Data", x = "LDA Component 1", y = "LDA Component 2", color = "Source Groups") +
  scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#bcbd22", "#1f6e66", "#6a2d92")) +
  
  annotate(
    "text", 
    x = Inf, y = -Inf, label = paste("LDA Model Accuracy:", round(accuracy_edxrf, 2)), 
    hjust = 1.1, vjust = -0.5, size = 4, fontface = "italic", color = "black"
  )


