#LAICPMS DATASETS WITH TOP 15 FEATURES SELECTED 

#LDA 

#LOAD IN DATA
laicpms_top15 <- read.csv("LAICPMS DATA/subset_data_with_selected_features_top15.csv")

colnames(laicpms_top15)


##############
#Only CBZ material 
laicpms_top15_NE <- subset(laicpms_top15, !(Group %in% c("23G-VA", "La Milpa N", "RBR04", "La Milpa Quarry", "24G-G", "24G-H", "24G-I", "24G-J", "24G-N", "24G-T", "RBR01", "RBR02", "RBR03")))

# Remove the ANID column from the dataset
laicpms_top15_NE$ANID <- NULL

#split data into training (80%) and test set (20%)
set.seed(2025)


training.samples_laicpms_top15_NE <- laicpms_top15_NE$Group %>%
  createDataPartition(p = 0.8, list = FALSE)

train.data_laicpms_top15_NE <- laicpms_top15_NE[training.samples_laicpms_top15_NE, ]
test.data_laicpms_top15_NE <- laicpms_top15_NE[-training.samples_laicpms_top15_NE, ]

# Normalize the data 
# Estimate preprocessing parameters
preproc.param_laicpms_top15_NE <- train.data_laicpms_top15_NE%>% 
  preProcess(method = c("center", "scale"))

# Transform the data using the estimated parameters
train.transformed_laicpms_top15_NE <- predict(preproc.param_laicpms_top15_NE, train.data_laicpms_top15_NE)
test.transformed_laicpms_top15_NE <- predict(preproc.param_laicpms_top15_NE, test.data_laicpms_top15_NE)

# Check the structure of the transformed data to ensure it looks good
str(train.transformed_laicpms_top15_NE)
str(test.transformed_laicpms_top15_NE)

#Perform LDA
lda_laicpms_top15_NE <- lda(Group ~ ., data = train.transformed_laicpms_top15_NE)

# Make predictions
test.prediction_laicpms_top15_NE <- predict(lda_laicpms_top15_NE, test.transformed_laicpms_top15_NE)


# Model accuracy
accuracy_laicpms_top15_NE <- mean(test.prediction_laicpms_top15_NE$class == test.transformed_laicpms_top15_NE$Group)
print(paste("Model accuracy: ", accuracy_laicpms_top15_NE))

# Predicted classes
head(test.prediction_laicpms_top15_NE$class, 6)
# Predicted probabilities of class membership.
head(test.prediction_laicpms_top15_NE$posterior, 6) 
# Linear discriminants
head(test.prediction_laicpms_top15_NE$x, 3) 

lda.data_laicpms_top15_NE <- cbind(train.transformed_laicpms_top15_NE, predict(lda_laicpms_top15_NE)$x)

ggplot(lda.data_laicpms_top15_NE, aes(LD1, LD2)) +
  geom_point(alpha = 1, aes(color = Group)) +
  stat_ellipse(aes(color = Group)) +
  labs(title = "LDA of LAICPMS Data", x = "LDA Component 1", y = "LDA Component 2", color = "Source Groups", caption = "Feature importance = 15") +
  scale_color_manual(values = c("red", "blue", "green", "yellow", "orange", "purple", "pink", "brown", "gray", 
                                "black", "white", "cyan", "magenta", "lightblue", "darkgreen", "gold", 
                                "beige", "salmon", "peachpuff", "#E6E6FA", "darkblue", "turquoise")) +  # Hex code for lavender and dark blue
  annotate(
    "text", 
    x = Inf, y = -Inf, label = paste("LDA Model Accuracy:", round(accuracy_laicpms_top15_NE, 10)), 
    hjust = 1.1, vjust = -0.5, size = 4, fontface = "italic", color = "black"
  )

#####################
#RIOBRAVO 

laicpms_top15_NW <- subset(laicpms_top15, !(Group %in% c("Rancho", "23G-AA", "23G-BA", "23G-F", "23G-E", "23G-I", "23G-V", "23G-W", "23G-X", "23G-Y", "23G-Z", "24G-J")))

View(laicpms_top15_NW)
# Remove the ANID column from the dataset
laicpms_top15_NW$ANID <- NULL

#split data into training (80%) and test set (20%)
set.seed(2025)


training.samples_laicpms_top15_NW <- laicpms_top15_NW$Group %>%
  createDataPartition(p = 0.8, list = FALSE)

train.data_laicpms_top15_NW <- laicpms_top15_NW[training.samples_laicpms_top15_NW, ]
test.data_laicpms_top15_NW <- laicpms_top15_NW[-training.samples_laicpms_top15_NW, ]

# Normalize the data 
# Estimate preprocessing parameters
preproc.param_laicpms_top15_NW <- train.data_laicpms_top15_NW%>% 
  preProcess(method = c("center", "scale"))

# Transform the data using the estimated parameters
train.transformed_laicpms_top15_NW <- predict(preproc.param_laicpms_top15_NW, train.data_laicpms_top15_NW)
test.transformed_laicpms_top15_NW <- predict(preproc.param_laicpms_top15_NW, test.data_laicpms_top15_NW)

# Check the structure of the transformed data to ensure it looks good
str(train.transformed_laicpms_top15_NW)
str(test.transformed_laicpms_top15_NW)

View(train.data_laicpms_top15_NW)

#Perform LDA
lda_laicpms_top15_NW <- lda(Group ~ ., data = train.transformed_laicpms_top15_NW)

# Make predictions
test.prediction_laicpms_top15_NW <- predict(lda_laicpms_top15_NW, test.transformed_laicpms_top15_NW)


# Model accuracy
accuracy_laicpms_top15_NW <- mean(test.prediction_laicpms_top15_NW$class == test.transformed_laicpms_top15_NW$Group)
print(paste("Model accuracy: ", accuracy_laicpms_top15_NW))

# Predicted classes
head(test.prediction_laicpms_top15_NW$class, 6)
# Predicted probabilities of class membership.
head(test.prediction_laicpms_top15_NW$posterior, 6) 
# Linear discriminants
head(test.prediction_laicpms_top15_NW$x, 3) 

lda.data_laicpms_top15_NW <- cbind(train.transformed_laicpms_top15_NW, predict(lda_laicpms_top15_NW)$x)

ggplot(lda.data_laicpms_top15_NW, aes(LD1, LD2)) +
  geom_point(alpha = 1, aes(color = Group)) +
  stat_ellipse(aes(color = Group)) +
  labs(title = "LDA of LAICPMS Data", x = "LDA Component 1", y = "LDA Component 2", color = "Source Groups") +
  scale_color_manual(values = c("red", "blue", "green", "yellow", "orange", "purple", "pink", "brown", "gray", 
                                "black", "white", "cyan", "magenta", "lightblue", "darkgreen", "gold", 
                                "beige", "salmon", "peachpuff", "#E6E6FA", "darkblue", "turquoise")) +  # Hex code for lavender and dark blue
  annotate(
    "text", 
    x = Inf, y = -Inf, label = paste("LDA Model Accuracy:", round(accuracy_laicpms_top15_NW, 2)), 
    hjust = 1.1, vjust = -0.5, size = 4, fontface = "italic", color = "black"
  )
