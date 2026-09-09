#----------------------------------------------
# LDA COMPARISON: NAA vs. LA-ICP-MS
# CLR + Standardization
#----------------------------------------------

#----------------------------------------------
# Load packages
#----------------------------------------------

if (!require(MASS)) install.packages("MASS", dependencies = TRUE)
if (!require(caret)) install.packages("caret", dependencies = TRUE)
if (!require(dplyr)) install.packages("dplyr", dependencies = TRUE)
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)
if (!require(compositions)) install.packages("compositions", dependencies = TRUE)
if (!require(zCompositions)) install.packages("zCompositions", dependencies = TRUE)

library(MASS)
library(caret)
library(dplyr)
library(ggplot2)
library(compositions)
library(zCompositions)


#----------------------------------------------
# Load data
#----------------------------------------------
naa <- read.csv(
  "Technique Comparison/INAA_comparison_dataset.csv"
)

laicpms <- read.csv(
  "Technique Comparison/LAICPMS_comparison_dataset.csv"
)


#----------------------------------------------
# Remove non-geochemical/coloring variables
#----------------------------------------------

# These elements were excluded because they primarily
# represent coloration rather than useful provenance variation.

remove_elements <- c(
  "SiO2",
  "CaCO3",
  "Ni",
  "Fe2O3",
  "Cr",
  "Mn"
)

naa <- naa[
  , !(names(naa) %in% remove_elements)
]

laicpms <- laicpms[
  , !(names(laicpms) %in% remove_elements)
]


# Remove sample ID if present
if ("ANID" %in% names(naa)) {
  naa$ANID <- NULL
}

if ("ANID" %in% names(laicpms)) {
  laicpms$ANID <- NULL
}

#----------------------------------------------
# Function to prepare CLR data
#----------------------------------------------

prepare_clr_data <- function(data, group_col = "Group") {
  
  # Check group column
  if (!group_col %in% names(data)) {
    stop("Group column not found in dataset.")
  }
  
  # Store group information
  group <- as.factor(data[[group_col]])
  
  # Remove group column before compositional analysis
  predictors <- data[
    , !(names(data) %in% group_col),
    drop = FALSE
  ]
  
  # Keep numeric variables only
  predictors <- predictors[
    , sapply(predictors, is.numeric),
    drop = FALSE
  ]
  
  # Check for remaining non-geochemical numeric variables
  message(
    "Number of numeric variables entering CLR: ",
    ncol(predictors)
  )
  
  #--------------------------------------------------------
  # Multiplicative zero replacement
  #--------------------------------------------------------
  
  predictors_replaced <- cmultRepl(
    predictors,
    label = 0,
    method = "CZM"
  )
  
  #--------------------------------------------------------
  # CLR transformation
  #--------------------------------------------------------
  
  clr_data <- clr(
    acomp(predictors_replaced)
  )
  
  clr_data <- as.data.frame(clr_data)
  
  # Restore variable names
  colnames(clr_data) <- colnames(predictors)
  
  # Add grouping variable
  clr_data[[group_col]] <- group
  
  return(clr_data)
}


#----------------------------------------------
# Prepare NAA and LA-ICP-MS CLR datasets
#----------------------------------------------

clr_naa <- prepare_clr_data(
  naa,
  group_col = "Group"
)

clr_laicpms <- prepare_clr_data(
  laicpms,
  group_col = "Group"
)

#----------------------------------------------
# LDA function
#----------------------------------------------

run_lda_clr <- function(
    data,
    group_col = "Group",
    split_ratio = 0.80,
    seed = 123,
    plot_title = "LDA Results"
) {
  
  #--------------------------------------------------------
  # Reproducibility
  #--------------------------------------------------------
  
  set.seed(seed)
  
  #--------------------------------------------------------
  # Ensure grouping variable is a factor
  #--------------------------------------------------------
  
  data[[group_col]] <- as.factor(
    data[[group_col]]
  )
  
  # Remove incomplete observations
  data <- data[
    complete.cases(data),
    ,
    drop = FALSE
  ]
  
  #--------------------------------------------------------
  # Train/test split
  #--------------------------------------------------------
  
  training.samples <- createDataPartition(
    data[[group_col]],
    p = split_ratio,
    list = FALSE
  )
  
  train.data <- data[
    training.samples,
    ,
    drop = FALSE
  ]
  
  test.data <- data[
    -training.samples,
    ,
    drop = FALSE
  ]
  
  #--------------------------------------------------------
  # Predictor names
  #--------------------------------------------------------
  
  predictors <- setdiff(
    names(train.data),
    group_col
  )
  
  #--------------------------------------------------------
  # Remove near-zero variance variables
  #
  # IMPORTANT:
  # This is calculated using TRAINING DATA ONLY.
  #--------------------------------------------------------
  
  nzv <- nearZeroVar(
    train.data[, predictors, drop = FALSE],
    saveMetrics = TRUE
  )
  
  remove_vars <- rownames(
    nzv[nzv$nzv == TRUE, ]
  )
  
  if (length(remove_vars) > 0) {
    
    message(
      "Removed ",
      length(remove_vars),
      " near-zero variance variables."
    )
    
    train.data <- train.data[
      ,
      !(names(train.data) %in% remove_vars),
      drop = FALSE
    ]
    
    test.data <- test.data[
      ,
      !(names(test.data) %in% remove_vars),
      drop = FALSE
    ]
  }
  
  #--------------------------------------------------------
  # Standardization
  #
  # Centering and scaling parameters are calculated from
  # TRAINING DATA ONLY and then applied to both datasets.
  #--------------------------------------------------------
  
  predictors <- setdiff(
    names(train.data),
    group_col
  )
  
  preproc.param <- preProcess(
    train.data[, predictors, drop = FALSE],
    method = c("center", "scale")
  )
  
  train_scaled <- predict(
    preproc.param,
    train.data[, predictors, drop = FALSE]
  )
  
  test_scaled <- predict(
    preproc.param,
    test.data[, predictors, drop = FALSE]
  )
  
  # Add grouping variable back
  train_scaled[[group_col]] <- train.data[[group_col]]
  
  test_scaled[[group_col]] <- test.data[[group_col]]
  
  #--------------------------------------------------------
  # LDA model
  #--------------------------------------------------------
  
  lda_formula <- as.formula(
    paste(group_col, "~ .")
  )
  
  lda.model <- lda(
    lda_formula,
    data = train_scaled
  )
  
  #--------------------------------------------------------
  # Predict held-out test data
  #--------------------------------------------------------
  
  predictions <- predict(
    lda.model,
    test_scaled
  )
  
  predicted_classes <- factor(
    predictions$class,
    levels = levels(train_scaled[[group_col]])
  )
  
  true_classes <- factor(
    test_scaled[[group_col]],
    levels = levels(train_scaled[[group_col]])
  )
  
  #--------------------------------------------------------
  # Confusion matrix
  #--------------------------------------------------------
  
  cm <- confusionMatrix(
    data = predicted_classes,
    reference = true_classes
  )
  
  #--------------------------------------------------------
  # Overall accuracy
  #--------------------------------------------------------
  
  accuracy <- as.numeric(
    cm$overall["Accuracy"]
  )
  
  #--------------------------------------------------------
  # Per-class metrics
  #--------------------------------------------------------
  
  by_class <- cm$byClass
  
  # Convert binary output to matrix if necessary
  if (is.null(dim(by_class))) {
    by_class <- t(
      as.data.frame(by_class)
    )
  }
  
  #--------------------------------------------------------
  # Calculate weighted metrics
  #--------------------------------------------------------
  
  # Number of observations in each TRUE class
  class_counts <- table(true_classes)
  
  class_weights <- class_counts / sum(class_counts)
  
  # Make sure weights correspond to metric rows
  metric_classes <- rownames(by_class)
  
  metric_class_names <- gsub(
    "^Class: ",
    "",
    metric_classes
  )
  
  weights <- class_weights[
    metric_class_names
  ]
  
  weights[is.na(weights)] <- 0
  
  weighted_precision <- sum(
    by_class[, "Precision"] * weights,
    na.rm = TRUE
  )
  
  weighted_recall <- sum(
    by_class[, "Recall"] * weights,
    na.rm = TRUE
  )
  
  weighted_f1 <- sum(
    by_class[, "F1"] * weights,
    na.rm = TRUE
  )
  
  #--------------------------------------------------------
  # LDA scores for training data
  #--------------------------------------------------------
  
  lda_train <- predict(
    lda.model,
    train_scaled
  )
  
  lda_scores <- as.data.frame(
    lda_train$x
  )
  
  lda_scores[[group_col]] <-
    train_scaled[[group_col]]
  
  #--------------------------------------------------------
  # LDA plot
  #--------------------------------------------------------
  
  p <- ggplot(
    lda_scores,
    aes(
      x = LD1,
      y = LD2,
      color = .data[[group_col]],
      fill = .data[[group_col]]
    )
  ) +
    
    stat_ellipse(
      geom = "polygon",
      alpha = 0.10,
      color = NA
    ) +
    
    stat_ellipse(
      linewidth = 0.4
    ) +
    
    geom_point(
      alpha = 0.65,
      size = 1.5
    ) +
    
    labs(
      title = plot_title,
      x = "LDA Component 1",
      y = "LDA Component 2",
      color = "Sample Group",
      fill = "Sample Group"
    ) +
    
    scale_color_manual(
      values = c(
        "red",
        "blue",
        "orange",
        "purple",
        "darkgreen",
        "lightblue"
      )
    ) +
    
    scale_fill_manual(
      values = c(
        "red",
        "blue",
        "orange",
        "purple",
        "darkgreen",
        "lightblue"
      )
    ) +
    
    theme_grey()
  
  #--------------------------------------------------------
  # Return results
  #--------------------------------------------------------
  
  return(
    list(
      
      model = lda.model,
      
      accuracy = accuracy,
      
      weighted_precision =
        weighted_precision,
      
      weighted_recall =
        weighted_recall,
      
      weighted_f1 =
        weighted_f1,
      
      confusion_matrix = cm,
      
      predictions = predictions,
      
      train_data = train_scaled,
      
      test_data = test_scaled,
      
      lda_scores = lda_scores,
      
      plot = p,
      
      # Number of variables actually used by LDA
      n_variables = length(predictors),
      
      variables = predictors,
      
      removed_nzv = remove_vars
    )
  )
}


#----------------------------------------------
# Run NAA LDA
#----------------------------------------------

results_naa <- run_lda_clr(
  clr_naa,
  group_col = "Group",
  split_ratio = 0.80,
  seed = 123,
  plot_title = "LDA of NAA Data"
)


#----------------------------------------------
# Run LA-ICP-MS LDA
#----------------------------------------------

results_laicpms <- run_lda_clr(
  clr_laicpms,
  group_col = "Group",
  split_ratio = 0.80,
  seed = 123,
  plot_title = "LDA of LA-ICP-MS Data"
)


#----------------------------------------------
# View individual results
#----------------------------------------------

# NAA
results_naa$accuracy
results_naa$weighted_precision
results_naa$weighted_recall
results_naa$weighted_f1
results_naa$confusion_matrix
results_naa$model
results_naa$plot


# LA-ICP-MS
results_laicpms$accuracy
results_laicpms$weighted_precision
results_laicpms$weighted_recall
results_laicpms$weighted_f1
results_laicpms$confusion_matrix
results_laicpms$model
results_laicpms$plot


#----------------------------------------------
# Per-class performance
#----------------------------------------------

# NAA
round(
  results_naa$confusion_matrix$byClass[
    ,
    c("Precision", "Recall", "F1")
  ],
  3
)


# LA-ICP-MS
round(
  results_laicpms$confusion_matrix$byClass[
    ,
    c("Precision", "Recall", "F1")
  ],
  3
)


#----------------------------------------------
# Direct NAA vs. LA-ICP-MS comparison
#----------------------------------------------

comparison_table <- data.frame(
  
  Method = c(
    "NAA",
    "LA-ICP-MS"
  ),
  
  Accuracy = c(
    results_naa$accuracy,
    results_laicpms$accuracy
  ),
  
  Weighted_Precision = c(
    results_naa$weighted_precision,
    results_laicpms$weighted_precision
  ),
  
  Weighted_Recall = c(
    results_naa$weighted_recall,
    results_laicpms$weighted_recall
  ),
  
  Weighted_F1 = c(
    results_naa$weighted_f1,
    results_laicpms$weighted_f1
  ),
  
  Number_of_Variables = c(
    results_naa$n_variables,
    results_laicpms$n_variables
  )
)


# Round metrics
comparison_table[
  ,
  c(
    "Accuracy",
    "Weighted_Precision",
    "Weighted_Recall",
    "Weighted_F1"
  )
] <- round(
  comparison_table[
    ,
    c(
      "Accuracy",
      "Weighted_Precision",
      "Weighted_Recall",
      "Weighted_F1"
    )
  ],
  3
)


# Print comparison
print(comparison_table)

