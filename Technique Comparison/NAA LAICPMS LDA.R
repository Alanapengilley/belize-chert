#----------------------------------------------------------
# Linear Discriminant Analysis (LDA) of NAA and LAICPMS 
#----------------------------------------------------------

#Load in necessary packages 
library(readxl)
library(dplyr)
library(MASS)
library(caret)
library(ggplot2)
library(writexl)

#----------------------------------------------------------
# Load Data
#----------------------------------------------------------
naa <- read.csv("Technique Comparison/INAA_comparison_dataset.csv")
laicpms <- read.csv("Technique Comparison/LAICPMS_comparison_dataset.csv")

#----------------------------------------------------------
# Remove coloring elements
#----------------------------------------------------------
naa <- naa[, !(names(naa) %in% c("SiO2", "CaCO3", "Ni", "Fe2O3", "Cr", "Mn"))]
laicpms <- laicpms[, !(names(laicpms) %in% c("SiO2", "CaCO3", "Ni", "Fe2O3", "Cr", "Mn"))]


#----------------------------------------------------------
# Function to run LDA
#----------------------------------------------------------
run_lda_model <- function(data, group_col = "Group", split_ratio = 0.8, plot_title = "LDA Results") {
  library(MASS)
  library(caret)
  library(ggplot2)
  library(dplyr)
  
  # --- Ensure reproducibility ---
  set.seed(123)
  
  # --- Check input ---
  if(!group_col %in% names(data)) stop("Group column not found in dataset.")
  
  # --- Data cleaning ---
  data <- data[, !(is.na(names(data)) | names(data) == "")]
  data <- data[, colSums(!is.na(data)) > 0]
  
  # Remove constant / near-zero variance variables
  predictors <- setdiff(names(data), group_col)
  nzv <- nearZeroVar(data[, predictors], saveMetrics = TRUE)
  const_vars <- rownames(nzv[nzv$nzv == TRUE, ])
  if (length(const_vars) > 0) {
    message("Removed ", length(const_vars), " near-zero variance variables.")
    data <- data[, !(names(data) %in% const_vars)]
  }
  
  # --- Ensure group column is a factor ---
  data[[group_col]] <- as.factor(make.names(data[[group_col]]))
  
  # --- Train/test split ---
  training.samples <- data[[group_col]] %>%
    createDataPartition(p = split_ratio, list = FALSE)
  
  train.data <- data[training.samples, ]
  test.data  <- data[-training.samples, ]
  
  # --- Normalize numeric predictors ---
  preproc.param <- train.data %>% preProcess(method = c("center", "scale"))
  train.transformed <- predict(preproc.param, train.data)
  test.transformed  <- predict(preproc.param, test.data)
  
  # --- Fit LDA model ---
  formula <- as.formula(paste(group_col, "~ ."))
  lda.model <- lda(formula, data = train.transformed)
  
  # --- Predict ---
  predictions <- predict(lda.model, test.transformed)
  predicted_classes <- factor(predictions$class, levels = levels(train.transformed[[group_col]]))
  true_classes <- factor(test.transformed[[group_col]], levels = levels(train.transformed[[group_col]]))
  
  # --- Handle mismatch in factor levels ---
  common_levels <- intersect(levels(true_classes), levels(predicted_classes))
  predicted_classes <- factor(predicted_classes, levels = common_levels)
  true_classes <- factor(true_classes, levels = common_levels)
  
  # --- Confusion matrix and metrics ---
  cm <- confusionMatrix(predicted_classes, true_classes)
  accuracy <- cm$overall["Accuracy"]
  
  # --- Weighted metrics (binary OR multiclass safe) ---
  by_class <- cm$byClass
  
  # If binary, make it a 1-row data frame
  if (is.null(dim(by_class))) {
    by_class <- t(as.data.frame(by_class))
  }
  
  # Compute weighted metrics
  class_counts <- table(true_classes)
  class_weights <- class_counts / sum(class_counts)
  precision_per_class <- by_class[, "Precision"]
  recall_per_class    <- by_class[, "Recall"]
  f1_per_class        <- by_class[, "F1"]
  
  weighted_precision <- sum(precision_per_class * class_weights, na.rm = TRUE)
  weighted_recall    <- sum(recall_per_class * class_weights, na.rm = TRUE)
  weighted_f1        <- sum(f1_per_class * class_weights, na.rm = TRUE)
  
  # --- Plot ---
  lda.data <- cbind(train.transformed, predict(lda.model)$x)
  
  p <- ggplot(lda.data, aes(LD1, LD2, color = .data[[group_col]], fill = .data[[group_col]])) +
    # Filled, semi-transparent ellipses
    stat_ellipse(geom = "polygon", alpha = 0.1, size = 0.5, color = NA) +
    # Outline of ellipses (same color as points)
    stat_ellipse(size = 0.4) +
    # Points
    geom_point(alpha = 0.6, size = 1) +
    labs(
      title = plot_title,
      x = "LDA Component 1",
      y = "LDA Component 2",
      color = "Groups",
      fill = "Groups",
      caption = paste0(
        "Accuracy = ", round(accuracy, 4),
        " | Weighted F1 = ", round(weighted_f1, 4)
      )
    ) +
    scale_color_manual(values = c(
      "red", "blue", "orange", "purple", "darkgreen", "lightblue"
    )) +
    scale_fill_manual(values = c(
      "red", "blue", "orange", "purple", "darkgreen", "lightblue"
    )) +
    theme_grey() +
    theme(
      legend.position = "right"
    )
  
  # --- Return ---
  list(
    model = lda.model,
    accuracy = as.numeric(accuracy),
    weighted_precision = as.numeric(weighted_precision),
    weighted_recall = as.numeric(weighted_recall),
    weighted_f1 = as.numeric(weighted_f1),
    confusion_matrix = cm,
    predictions = predictions,
    train_data = train.transformed,
    test_data = test.transformed,
    plot = p
  )
}


#----------------------------------------------------------
# Run NAA data
#----------------------------------------------------------
# Remove the ANID column from the dataset
naa$ANID <- NULL

results_naa <- run_lda_model(naa, group_col = "Group", split_ratio = 0.8, plot_title = "LDA of NAA Data")

# View results
results_naa$accuracy
results_naa$weighted_f1
results_naa$confusion_matrix

# View LDA plot
results_naa$plot

# View model details
results_naa$model

results_naa$confusion_matrix
results_naa$confusion_matrix$byClass
#shows results per group
round(results_naa$confusion_matrix$byClass[, c("Precision", "Recall", "F1")], 20)


#----------------------------------------------------------
# Run LAICPMS data
#----------------------------------------------------------
# Remove the ANID column from the dataset
laicpms$ANID <- NULL
results2 <- run_lda_model(laicpms, group_col = "Group", split_ratio = 0.8, plot_title = "LDA of LA-ICP-MS Data")

# View results
results2$accuracy
results2$weighted_f1
results2$confusion_matrix

# View LDA plot
results2$plot

# View model details
results2$model


