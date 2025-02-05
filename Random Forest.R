
#### Random Forest (RF) Classifier
#Train Random Forest model with k-fold stratified cross-validation for multi-class classsificaton 

install.packages("randomForest")
library(randomForest)
library(readxl)
library(caret)


chert <- read_excel("Geology LAICPMS.xlsx") 
head(chert)

chert <- chert[, !names(chert) %in% c("ANID")] 

print(chert)

#ensure the target class variable is a factor
chert$Group <- as.factor(chert$Group)
table(chert$Group)

# Clean the class levels by making them valid R variable names
levels(chert$Group) <- make.names(levels(chert$Group))
# Check if the levels have been fixed
levels(chert$Group)

set.seed(123)  # For reproducibility

# Define control parameters for cross-validation
train_control <- trainControl(method = "cv",            # Cross-validation method
                              number = 10,             # Number of folds (10-fold CV)
                              classProbs = TRUE,       # Calculate class probabilities (for ROC, etc.)
                              summaryFunction = defaultSummary) # Used for binary classification metrics (use confusionMatrix for multiclass)


# Train the Random Forest model
rf_model <- train(Group ~ .,          # Target variable ~ predictors
                  data = chert,       # The dataset
                  method = "rf",      # Random Forest method
                  trControl = train_control,  # Cross-validation settings
                  metric = "Accuracy")       # Metric to optimize (you can use others like 'ROC' for binary classification)

# Print model summary
print(rf_model)

# Get the cross-validation results (e.g., accuracy)
rf_model$results

# Confusion matrix
confusionMatrix(rf_model)

#feature importance 
importance_scores <- importance(rf_model$finalModel)

barplot(importance_scores[, 1], 
        main = "Feature Importance", 
        col = "lightblue", 
        las = 2, 
        cex.names = 0.7)


##### RFECV
# Load necessary libraries
library(caret)
library(MASS)  # For LDA
library(e1071)  # For SVM

set.seed(123)

# Step 1: Perform RFECV for feature selection using Random Forest
train_control <- trainControl(method = "cv", 
                              number = 10, 
                              savePredictions = "all", 
                              returnResamp = "all", 
                              classProbs = TRUE)

# Train a Random Forest model with RFECV (recursive feature elimination + cross-validation)
rfecv_model <- rfe(Group ~ ., 
                   data = chert, 
                   sizes = c(1:4),  # Try different number of features
                   rfeControl = rfeControl(functions = rfFuncs, method = "cv"))

# Print the results of RFECV (showing selected features)
print(rfecv_model)

# Step 2: Extract the selected features
selected_features <- rfecv_model$optVariables
print(selected_features)

# Subset the data with selected features
selected_data <- chert[, c(selected_features, "Group")]

# Step 3: Train a Linear Discriminant Analysis (LDA) model using selected features
lda_model <- lda(Group ~ ., data = selected_data)

# Step 4: Evaluate the LDA model's performance
lda_predictions <- predict(lda_model, selected_data)
confusionMatrix(lda_predictions$class, selected_data$Group)


#Extract the LDA scores (the projections onto LDA1 and LDA2)
lda_scores <- predict(lda_model)$x  # This gives the LDA1 and LDA2 components

lda_data <- data.frame(LDA1 = lda_scores[, 1],
                       LDA2 = lda_scores[, 2],
                       Class = train_data$Group)

ggplot(lda_predictions, aes(LD1, LD2)) +
  geom_point(aes(color = Group)) +
  stat_ellipse(aes(color = Group))

# introduce artifacts 
artifacts <- read_excel("Artifacts.xlsx") 
head(artifacts)

# Subset new data to only include the selected features (ensure the same columns)
new_data_subset <- artifacts[, selected_features]

# Make predictions using the trained LDA model
lda_predictions <- predict(lda_model, new_data_subset, type = "response")

# View the predicted probabilities for each class
print(lda_predictions$posterior)


# Create a data frame with names, predicted classes, and probabilities
prediction_table <- data.frame(
  Predicted_Class = lda_predictions$class,
  Predicted_Probability = apply(lda_predictions$posterior, 1, function(x) max(x))  # Prob of predicted class
)

# View the prediction table
print(prediction_table)


#####
cv_results <- rf_model$resample
print(cv_results)

# Get importance for each fold:
fold_importances <- list()


# Access fold indices from resample object
for (i in 1:nrow(cv_results)) {
  # Access the resample indices for fold 'i'
  fold_train_indices <- rf_model$pred[rf_model$pred$Resample == paste0("Fold", i),]
  
  # Use the indices to extract the training data for this fold
  fold_train_data <- chert[fold_train_indices$rowIndex, ]
  
  # Check if the fold has at least two classes before training
  if (length(unique(fold_train_data$Species)) > 1) {
    # Train a Random Forest model on this fold's data
    fold_rf_model <- randomForest(Group ~ ., data = fold_train_data, importance = TRUE)
    
    # Store the importance scores for this fold
    fold_importances[[i]] <- importance(fold_rf_model)
  } else {
    cat("Fold", i, "only contains one class, skipping.\n")
  }
}




library(ggplot2)
# Assuming 'importance_scores' is the output from randomForest::importance() and contains the importance values

# Convert the importance_scores to a data frame
importance_df <- as.data.frame(importance_scores)

# Normalize the importance (optional, relative to maximum)
importance_df$normalized_importance <- importance_df[, "MeanDecreaseGini"] / max(importance_df[, "MeanDecreaseGini"])

ggplot(importance_df, aes(x = reorder(rownames(importance_df), normalized_importance), 
                          y = normalized_importance, fill = normalized_importance)) +
  geom_boxplot(alpha = 0.3) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Rotate x-axis labels for better readability
  labs(title = "Feature Importance",
       x = "Features",
       y = "Normalized Importance") +
  theme_minimal()




