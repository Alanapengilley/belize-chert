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
# NAA: Prepare compositional data
#----------------------------------------------------------
# Remove non-geochemical/coloring variables
naa <- naa[, !(names(naa) %in%
                 c("SiO2", "CaCO3", "Ni", "Fe2O3", "Cr", "Mn"))]

# Remove ANID
naa$ANID <- NULL

# Store grouping variable
group <- as.factor(naa$Group)

# Select ONLY geochemical variables
geochemical_cols <- setdiff(
  names(naa),
  "Group"
)

numeric_naa <- naa[, geochemical_cols]

# Multiplicative zero replacement
naa_replaced <- cmultRepl(
  numeric_naa,
  label = 0,
  method = "CZM"
)

#CLR transformation

clr_naa <- clr(
  acomp(naa_replaced)
)

clr_naa <- as.data.frame(clr_naa)

colnames(clr_naa) <- colnames(numeric_naa)

# Add grouping variable
clr_naa$Group <- group



#----------------------------------------------------------
# Function to run LDA
#----------------------------------------------------------
run_lda_clr <- function(
    data,
    group_col = "Group",
    split_ratio = 0.8,
    plot_title = "LDA Results"
) {
  
  set.seed(123)
  
  # Ensure group is a factor
  data[[group_col]] <- as.factor(data[[group_col]])
  
  # Remove missing rows
  data <- data[complete.cases(data), ]
  
  #--------------------------------------------------------
  # Train/test split
  #--------------------------------------------------------
  
  training.samples <- createDataPartition(
    data[[group_col]],
    p = split_ratio,
    list = FALSE
  )
  
  train.data <- data[training.samples, ]
  test.data  <- data[-training.samples, ]
  
  #--------------------------------------------------------
  # Remove near-zero variance variables
  # BASED ON TRAINING DATA ONLY
  #--------------------------------------------------------
  
  predictors <- setdiff(
    names(train.data),
    group_col
  )
  
  nzv <- nearZeroVar(
    train.data[, predictors],
    saveMetrics = TRUE
  )
  
  remove_vars <- rownames(
    nzv[nzv$nzv == TRUE, ]
  )
  
  if (length(remove_vars) > 0) {
    
    train.data <- train.data[
      , !(names(train.data) %in% remove_vars)
    ]
    
    test.data <- test.data[
      , !(names(test.data) %in% remove_vars)
    ]
  }
  
  #--------------------------------------------------------
  # Standardize using TRAINING data only
  #--------------------------------------------------------
  
  preproc.param <- preProcess(
    train.data,
    method = c("center", "scale"),
    exclude = group_col
  )
  
  train.transformed <- predict(
    preproc.param,
    train.data
  )
  
  test.transformed <- predict(
    preproc.param,
    test.data
  )
  
  #--------------------------------------------------------
  # LDA
  #--------------------------------------------------------
  
  formula <- as.formula(
    paste(group_col, "~ .")
  )
  
  lda.model <- lda(
    formula,
    data = train.transformed
  )
  
  #--------------------------------------------------------
  # Prediction
  #--------------------------------------------------------
  
  predictions <- predict(
    lda.model,
    test.transformed
  )
  
  predicted_classes <- factor(
    predictions$class,
    levels = levels(train.transformed[[group_col]])
  )
  
  true_classes <- factor(
    test.transformed[[group_col]],
    levels = levels(train.transformed[[group_col]])
  )
  
  #--------------------------------------------------------
  # Confusion matrix
  #--------------------------------------------------------
  
  cm <- confusionMatrix(
    data = predicted_classes,
    reference = true_classes
  )
  
  #--------------------------------------------------------
  # LDA scores for plotting
  #--------------------------------------------------------
  
  lda_scores <- predict(
    lda.model,
    train.transformed
  )$x
  
  lda.data <- data.frame(
    lda_scores,
    Group = train.transformed[[group_col]]
  )
  
  #--------------------------------------------------------
  # Plot
  #--------------------------------------------------------
  
  p <- ggplot(
    lda.data,
    aes(
      x = LD1,
      y = LD2,
      color = Group,
      fill = Group
    )
  ) +
    stat_ellipse(
      geom = "polygon",
      alpha = 0.1,
      color = NA
    ) +
    stat_ellipse(
      linewidth = 0.4
    ) +
    geom_point(
      alpha = 0.6,
      size = 1.5
    ) +
    labs(
      title = plot_title,
      x = "LDA Component 1",
      y = "LDA Component 2",
      color = "Sample Group",
      fill = "Sample Group"
    ) +
    theme_grey()
  
  #--------------------------------------------------------
  # Return
  #--------------------------------------------------------
  
  list(
    model = lda.model,
    accuracy = as.numeric(cm$overall["Accuracy"]),
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
results_naa <- run_lda_clr(
  clr_naa,
  group_col = "Group",
  split_ratio = 0.8,
  plot_title = "LDA of NAA Data"
)

results_naa$accuracy
results_naa$confusion_matrix
results_naa$plot

# View model details
results_naa$model
results_naa$accuracy
results_naa$weighted_f1


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


