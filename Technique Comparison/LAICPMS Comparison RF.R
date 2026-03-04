#Random Forest of NAA Data and LAICPMS Data - technique Comparison 
#----------------------------------------------------------
# Load Libraries
#----------------------------------------------------------
# install.packages(c("randomForest", "caret", "dplyr", "readxl"))
library(randomForest)
library(caret)
library(dplyr)
library(readxl)
library(ggplot2)
library(writexl)
library(MLmetrics)

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
  
  cat("\n🔹 Transform:", transform, "| Balance:", balance, "\n")
  
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
    cat("⚠️ Removing tiny classes:", paste(small, collapse = ", "), "\n")
    data <- data %>% filter(!Group %in% small)
  }
  
  # Optional log10 transformation
  if (transform) {
    data <- data %>% mutate(across(where(is.numeric), ~ log10(. + 1)))
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
  acc <- cm$overall["Accuracy"]
  
  # Weighted metrics
  true <- test$Group
  pred <- preds
  
  # Calculate class-wise F1, precision, recall, weighted by class frequency
  class_counts <- table(true)
  class_weights <- class_counts / sum(class_counts)
  
  f1_scores <- sapply(levels(true), function(cls) {
    F1_Score(y_true = as.numeric(true == cls),
             y_pred = as.numeric(pred == cls))
  })
  
  precision_scores <- sapply(levels(true), function(cls) {
    Precision(y_true = as.numeric(true == cls),
              y_pred = as.numeric(pred == cls))
  })
  
  recall_scores <- sapply(levels(true), function(cls) {
    Recall(y_true = as.numeric(true == cls),
           y_pred = as.numeric(pred == cls))
  })
  
  weighted_f1 <- sum(f1_scores * class_weights)
  weighted_precision <- sum(precision_scores * class_weights)
  weighted_recall <- sum(recall_scores * class_weights)
  
  cat(sprintf("✅ Accuracy: %.2f%% | Weighted F1: %.3f\n", acc * 100, weighted_f1))
  
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
configs <- expand.grid(transform = c(FALSE, TRUE),
                       balance = c(FALSE, TRUE))

results <- lapply(1:nrow(configs), function(i) {
  cat("\n=============================\n")
  cat("Running configuration", i, "of", nrow(configs), "\n")
  cat("=============================\n")
  run_rf_test(laicpms,
              transform = configs$transform[i],
              balance = configs$balance[i])
})

#----------------------------------------------------------
# Summarize results
#----------------------------------------------------------
summary_df <- data.frame(
  Transform = sapply(results, function(x) ifelse(x$transform, "Log10", "Raw")),
  Balanced = sapply(results, function(x) ifelse(x$balance, "Yes", "No")),
  Accuracy = sapply(results, function(x) round(x$accuracy * 100, 2)),
  Weighted_F1 = sapply(results, function(x) round(x$f1_weighted, 3)),
  Weighted_Precision = sapply(results, function(x) round(x$precision_weighted, 3)),
  Weighted_Recall = sapply(results, function(x) round(x$recall_weighted, 3))
)

print(summary_df)

# Identify best model
best_idx <- which.max(sapply(results, function(x) x$f1_weighted))
cat("\n🏆 Best configuration (by weighted F1):\n")
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





