#----------------------------------------------------------
# Random Forest (RF) of LAICPMS Dataset 
#----------------------------------------------------------

# install.packages
library(randomForest)
library(caret)
library(dplyr)
library(readxl)
library(ggplot2)
library(writexl)
library(zCompositions)
library(compositions)

#----------------------------------------------------------
# Load Data
#----------------------------------------------------------
laicpms <- read.csv("Technique Comparison/LAICPMS_comparison_dataset.csv")

# Remove non-informative coloring elements
laicpms <- laicpms[, !(names(laicpms) %in% c("SiO2", "CaCO3", "Ni", "Fe2O3", "Cr", "Mn"))]

# Make sure Group is factor
laicpms$Group <- as.factor(laicpms$Group)
laicpms$Group <- as.factor(make.names(laicpms$Group))

# Remove sample ID column
if ("ANID" %in% names(laicpms)) {
  laicpms <- laicpms %>% dplyr::select(-ANID)
}


#----------------------------------------------------------
# Function to run Random Forest with weighted F1
#----------------------------------------------------------
run_rf_test <- function(data, transform = FALSE, balance = FALSE, seed = 123) {
  
  cat("\nTransform:", transform, "| Balance:", balance, "\n")
  
  # Ensure target is factor
  data$Group <- as.factor(make.names(data$Group))
  
  # Keep numeric predictors + Group
  numeric_cols <- sapply(data, is.numeric)
  data <- data[, numeric_cols | names(data) == "Group"]
  
  # Remove all-NA or zero-variance columns
  data <- as_tibble(data)
  nzv <- nearZeroVar(dplyr::select(data, -Group))
  if (length(nzv) > 0) data <- data[, -nzv]
  
  # Drop rows with NA
  data <- na.omit(data)
  
  # Drop classes with <3 samples
  tbl <- table(data$Group)
  if (any(tbl < 3)) {
    small <- names(tbl[tbl < 3])
    cat("Removing tiny classes:", paste(small, collapse = ", "), "\n")
    data <- data %>% filter(!Group %in% small)
  }
  
  # apply selected transformation
  if (transform == "Log10") {
    
    # Log10 transformation
    numeric_data <- data %>% dplyr::select(-Group)
    
    numeric_data <- numeric_data %>%
      mutate(across(where(is.numeric), ~ log10(. + 1)))
    
    group <- data$Group
    
    numeric_data$Group <- group
    data <- numeric_data
    
  } else if (transform == "CLR") {
    
    group <- data$Group
    
    numeric_data <- data %>% dplyr::select(-Group)
   
    numeric_replaced <- cmultRepl(numeric_data, label = 0, method = "CZM")
    
    clr_data <- clr(acomp(numeric_replaced))
     
    clr_data <- as.data.frame(clr_data)
     
     # Restore variable names
    colnames(clr_data) <- colnames(numeric_data)
     
     # Add Group back
    clr_data$Group <- group
     
     # Replace original data
    data <- clr_data
    
  } else if (transform == "Raw") {
    
    # No transformation
    data <- data
    
  } else {
    
    stop("transform must be 'Raw', 'Log10', or 'CLR'")
    
  }
  
  # Train/test split
  set.seed(seed)
  idx <- createDataPartition(data$Group, p = 0.8, list = FALSE)
  
  train <- data[idx, ]
  test  <- data[-idx, ]
  
  cat("Class balance in training set:\n")
  print(table(train$Group))
  
  # Cross-validation setup
  if (balance) {
    ctrl <- trainControl(
      method = "repeatedcv",
      number = 5,
      repeats = 2,
      classProbs = TRUE,
      sampling = "up"
    )
  } else {
    ctrl <- trainControl(
      method = "repeatedcv",
      number = 5,
      repeats = 2,
      classProbs = TRUE
    )
  }
  
  # Safe mtry grid
  max_mtry <- min(ncol(train) - 1, floor(sqrt(ncol(train) - 1)))
  tune_grid <- expand.grid(mtry = 1:max(1, max_mtry))
  
  # Train Random Forest
  set.seed(seed)
  rf_model <- train(
    Group ~ .,
    data = train,
    method = "rf",
    trControl = ctrl,
    tuneGrid = tune_grid,
    importance = TRUE,
    ntree = 1000,
    metric = "Accuracy"
  )
  
  # Predict on test set
  preds <- predict(rf_model, newdata = test)
  
  cm <- confusionMatrix(preds, test$Group)
  
  acc <- as.numeric(cm$overall["Accuracy"])
  
  # Weighted metrics
  true <- test$Group
  pred <- preds
  
  # Extract class-level metrics from confusion matrix
  by_class <- cm$byClass
  
  # Make sure results are handled consistently for multiclass data
  if (is.null(dim(by_class))) {
    
    precision_scores <- as.numeric(by_class["Pos Pred Value"])
   
    recall_scores <- as.numeric(by_class["Sensitivity"])
    
    f1_scores <- 2 * precision_scores * recall_scores /
      (precision_scores + recall_scores)
    
    class_counts <- table(true)
    class_weights <- as.numeric(class_counts) / sum(class_counts)
    
  } else {
    
    precision_scores <- by_class[, "Pos Pred Value"]
    recall_scores <- by_class[, "Sensitivity"]
    
    f1_scores <- 2 * precision_scores * recall_scores /
      (precision_scores + recall_scores)
    
    class_counts <- table(true)
    class_weights <- as.numeric(class_counts) / sum(class_counts)
  }
  
  # Remove undefined values if a class has no predicted observations
  valid <- is.finite(f1_scores) &
    is.finite(precision_scores) &
    is.finite(recall_scores)
  
  weighted_f1 <- sum(f1_scores[valid] * class_weights[valid]) /
    sum(class_weights[valid])
  
  weighted_precision <- sum(precision_scores[valid] * class_weights[valid]) /
    sum(class_weights[valid])
  
  weighted_recall <- sum(recall_scores[valid] * class_weights[valid]) /
    sum(class_weights[valid])
  
  cat(sprintf(
    "Accuracy: %.2f%% | Weighted F1: %.3f | Weighted Precision: %.3f | Weighted Recall: %.3f\n",
    acc * 100,
    weighted_f1,
    weighted_precision,
    weighted_recall
  ))
  
  # Variable importance
  var_imp <- varImp(rf_model, scale = TRUE)$importance
  if (!"Overall" %in% colnames(var_imp)) colnames(var_imp)[1] <- "Overall"
  var_imp$Variable <- rownames(var_imp)
  rownames(var_imp) <- NULL
  
  list(
    transform = transform,
    balance = balance,
    accuracy = acc,
    f1_weighted = weighted_f1,
    precision_weighted = weighted_precision,
    recall_weighted = weighted_recall,
    model = rf_model,
    test_set = test,
    test_preds = preds,
    confusion = cm,
    var_imp = var_imp
  )
}

#----------------------------------------------------------
# Run all configurations
#----------------------------------------------------------
configs <- expand.grid(
  transform = c("Raw", "Log10", "CLR"),
  balance = c(FALSE, TRUE),
  stringsAsFactors = FALSE)

results <- lapply(1:nrow(configs), function(i) {
  
  cat("\n=============================\n")
  cat(
    "Running configuration",
    i,
    "of",
    nrow(configs),
    "\n")
  cat("=============================\n")
  
  run_rf_test(
    laicpms,
    transform = as.character(configs$transform[i]),
    balance = configs$balance[i])
})

#----------------------------------------------------------
# Summarize results
#----------------------------------------------------------
summary_df <- data.frame(
  
  Transform = sapply(
    results,
    function(x) x$transform),
  
  Balanced = sapply(
    results,
    function(x) ifelse(x$balance, "Yes", "No")),
  
  Accuracy = sapply(
    results,
    function(x) round(x$accuracy * 100, 2)),
  
  Weighted_F1 = sapply(
    results,
    function(x) round(x$f1_weighted, 3)),
  
  Weighted_Precision = sapply(
    results,
    function(x) round(x$precision_weighted, 3)),
  
  Weighted_Recall = sapply(
    results,
    function(x) round(x$recall_weighted, 3))
)

print(summary_df)

# Identify best model
best_idx <- which.max(sapply(results, function(x) x$f1_weighted))
cat("\Best configuration (by weighted F1):\n")
print(summary_df[best_idx, ])
best_result <- results[[best_idx]]

#----------------------------------------------------------
# 📊 Confusion Matrix for best model
#----------------------------------------------------------
cm <- best_result$confusion
cm_df <- as.data.frame(cm$table)
colnames(cm_df) <- c("Predicted", "Actual", "Freq")

ggplot(cm_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), color = "black", size = 3) +
  scale_fill_gradient(low = "white", high = "orange") +
  labs(title = "LA-ICP-MS Confusion Matrix (Test Set)", x = "Actual Class", y = "Predicted Class") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#----------------------------------------------------------
# Top 20 Variable Importance
#----------------------------------------------------------
top_vars <- best_result$var_imp %>%
  arrange(desc(Overall)) %>%
  slice(1:20)

ggplot(top_vars, aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_col(fill = "orange") +
  coord_flip() +
  labs(title = "Top 20 Variable Importances (LA-ICP-MS)",
       x = "Variable",
       y = "Importance") +
  theme_minimal(base_size = 12)





