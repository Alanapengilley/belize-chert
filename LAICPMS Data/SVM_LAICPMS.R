#Support Vector Machine (SVM) Model 

install.packages("MLmetrics")

# Load required libraries
library(caret)
library(e1071)
library(MLmetrics)
library(dplyr)

# Load data
geological_data <- read.csv("LAICPMS DATA/subset_data_optimal_features.csv") 
head(geological_data)

geological_data <- subset(laicpms_optimal, !(Group %in% c("24G-J", "23G-VA")))

# Clean dataframe of unnecessary columns 
train_chert <- geological_data[, !names(geological_data) %in% c("ANID")] 

# Ensure the target class variable is a factor
train_chert$Group <- as.factor(train_chert$Group)

# Remove missing values
train_chert <- na.omit(train_chert)

# Clean the class levels
levels(train_chert$Group) <- make.names(levels(train_chert$Group))

# Log-transform numeric predictors
predictor_cols <- sapply(train_chert, is.numeric) & names(train_chert) != "Group"
train_chert[predictor_cols] <- log1p(train_chert[predictor_cols])

# Split into training and testing sets
set.seed(2025)
train_index <- createDataPartition(train_chert$Group, p = 0.8, list = FALSE)
train_data <- train_chert[train_index, ]
test_data <- train_chert[-train_index, ]

# Define cross-validation settings
train_control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              classProbs = TRUE,
                              summaryFunction = multiClassSummary)

# Train an SVM with radial basis function (RBF) kernel
svm_model <- train(Group ~ .,
                   data = train_data,
                   method = "svmRadial",  # Radial kernel SVM
                   trControl = train_control,
                   preProcess = c("center", "scale"),  # Standardize features
                   tuneLength = 5)  # Number of tuning parameter combinations

# View model summary
print(svm_model)
plot(svm_model)  # See performance across tuning parameters

svm_model$results

table(train_data$Group)

# Test model predictions on test set
svm_predictions <- predict(svm_model, newdata = test_data)
confusion_matrix <- confusionMatrix(svm_predictions, test_data$Group)
print(confusion_matrix)

# Optional: Predict on the full dataset for a sanity check
svm_predictions_all <- predict(svm_model, newdata = train_chert)
confusion_matrix_all <- confusionMatrix(svm_predictions_all, train_chert$Group)
print(confusion_matrix_all)
