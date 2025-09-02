##FEATURES SELECTED USING RECURSIVE FEATURE ELIMINATION WITH CROSS-VALIDATION
##LOF USED TO IDENTIFY IS ARTEFACTS ARE CONSIDERED OUTLIERS OR INLIERS COMPARED TO GEOLOGICAL DATASET 


# Install and load required packages
install.packages("caret")
install.packages("DMwR")
install.packages("e1071")
install.packages("randomForest")
install.packages("leaps")
install.packages("readxl")

# Load necessary libraries
library(Rlof)
library(ggplot2)
library(MASS)
library(caret)
library(DMwR)
library(e1071)
library(randomForest)
library(leaps)
library(randomForest)
library(readxl)

###########
##LOAD IN DATA 
#geological data set (from global environment)
df <- read_excel("LAICPMS DATA/Geology LAICPMS.xlsx") 
train_data <- df
#artifact data set
artifacts <- read_excel("Artifacts.xlsx") 
test_data <- artifacts

View(train_data)
View(df)

############
##Recursive Feature Elimination (RFE) with Cross-Validation

train_data <- train_data[, !names(train_data) %in% c("ANID")] # remove the 'ANID' column

#ensure the target class variable is a factor
train_data$Group <- as.factor(train_data$Group)
table(train_data$Group) #check number of classses in groups

str(train_data)  # Check the structure of the dataset

train_data <- na.omit(train_data)  # Remove rows with missing values

set.seed(2025) #reproducibility

# Create a custom summary function to calculate F1 score
f1_summary <- function(train_data, lev = NULL, model = NULL) {
  # Calculate F1 score
  precision <- posPredValue(train_data$pred, train_data$obs, positive = lev[1])
  recall <- sensitivity(train_data$pred, train_data$obs, positive = lev[1])
  
  f1 <- 2 * (precision * recall) / (precision + recall)
  return(c(F1 = f1))
}

# Perform RFECV for feature selection using Random Forest
train_control <- trainControl(method = "cv", 
                              number = 10, #10-fold cross-validation
                              savePredictions = "all", 
                              returnResamp = "all", 
                              classProbs = TRUE, 
                              summaryFunction = f1_summary)


# Train a Random Forest model with RFECV (recursive feature elimination + cross-validation)
rfecv_model <- rfe(Group ~ ., 
                   data = train_data, 
                   sizes = c(1:59),  # Try different number of features
                   rfeControl = rfeControl(functions = rfFuncs, 
                                           method = "cv", 
                                           verbose = TRUE,
                                           returnResamp = "all"))

# Print the results of RFECV (showing selected features)
print(rfecv_model)

print(rfecv_model$results)

head(rfecv_model$results)

# Convert the results to a data frame if needed
rfecv_results <- as.data.frame(rfecv_model$results)

#extarct final model from RFECV
final_rf_model <- rfecv_model$fit

print(final_rf_model)

#graph accuracy vs. number of features 
ggplot(rfecv_model$results, aes(x = Variables, y = Accuracy)) +
  geom_line(color = "steelblue", linewidth = 1) +  # using 'linewidth' instead of 'size'
  geom_point(color = "darkred", size = 2) +
  labs(title = "RFECV: Accuracy vs Number of Features",
       x = "Number of Features",
       y = "Accuracy") +
  theme_gray()


optimal_features <- rfecv_model$optVariables
print(paste("Optimal number of features:", length(optimal_features)))
print("Selected features:")
print(optimal_features)

#Extract results
# Access the selected features from the RFECV model
selected_features <- rfecv_model$optVariables

# Print selected features
cat("Selected features: ", selected_features, "\n")

# Select optimal features from the selected features list (if there are at least 15)
optimal_features <- selected_features[1:34]


# Subset the original data frame, including 'ANID', 'Group', and the top 15 features
subset_data <- df[, c('ANID', 'Group', optimal_features)]

# View the resulting data frame
View(subset_data)

# Optionally, if you want to save it as a CSV
write.csv(subset_data, "subset_data_optimal_features.csv", row.names = FALSE)


