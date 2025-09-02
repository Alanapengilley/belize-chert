#LAICPMS DATASETS WITH TOP 15 FEATURES SELECTED 

#LDA 

#LOAD IN DATA
laicpms_all <- read_excel("LAICPMS Data/Geology LAICPMS.xlsx")

colnames(laicpms_all)

num_columns <- ncol(laicpms_all)
print(num_columns)


###############
#ALL material 

laicpms_all <- subset(laicpms_all, !(Group %in% c("24G-J")))

# Remove the ANID column from the dataset
laicpms_all$ANID <- NULL

#split data into training (80%) and test set (20%)
set.seed(2025)

training.samples_laicpms_all <- laicpms_all$Group %>%
  createDataPartition(p = 0.8, list = FALSE)

train.data_laicpms_all <- laicpms_all[training.samples_laicpms_all, ]
test.data_laicpms_all <- laicpms_all[-training.samples_laicpms_all, ]

# Normalize the data 
# Estimate preprocessing parameters
preproc.param_laicpms_all <- train.data_laicpms_all%>% 
  preProcess(method = c("center", "scale"))

# Transform the data using the estimated parameters
train.transformed_laicpms_all <- predict(preproc.param_laicpms_all, train.data_laicpms_all)
test.transformed_laicpms_all <- predict(preproc.param_laicpms_all, test.data_laicpms_all)

# Check the structure of the transformed data to ensure it looks good
str(train.transformed_laicpms_all)
str(test.transformed_laicpms_all)

#Perform LDA
lda_laicpms_all <- lda(Group ~ ., data = train.transformed_laicpms_all)

# Make predictions
test.prediction_laicpms_all <- predict(lda_laicpms_all, test.transformed_laicpms_all)


# Model accuracy
accuracy_laicpms_all <- mean(test.prediction_laicpms_all$class == test.transformed_laicpms_all$Group)
print(paste("Model accuracy: ", accuracy_laicpms_all))

# Predicted classes
head(test.prediction_laicpms_all$class, 6)
# Predicted probabilities of class membership.
head(test.prediction_laicpms_all$posterior, 6) 
# Linear discriminants
head(test.prediction_laicpms_all$x, 3) 

lda.data_laicpms_all <- cbind(train.transformed_laicpms_all, predict(lda_laicpms_all)$x)

ggplot(lda.data_laicpms_all, aes(LD1, LD2)) +
  geom_point(alpha = 1, aes(color = Group)) +
  stat_ellipse(aes(color = Group)) +
  labs(title = "LDA of LAICPMS Data", x = "LDA Component 1", y = "LDA Component 2", color = "Source Groups", caption = "Feature importance = 59") +
  scale_color_manual(values = c("red", "blue", "green", "yellow", "orange", "purple", "pink", "brown", "gray", 
                                "black", "white", "cyan", "magenta", "lightblue", "darkgreen", "gold", 
                                "beige", "salmon", "peachpuff", "#E6E6FA", "darkblue", "turquoise")) +  # Hex code for lavender and dark blue
  annotate(
    "text", 
    x = Inf, y = -Inf, label = paste("LDA Model Accuracy:", round(accuracy_laicpms_all, 10)), 
    hjust = 1.1, vjust = -0.5, size = 4, fontface = "italic", color = "black"
  )


##############
#Only CBZ material 
laicpms_all_NE <- subset(laicpms_all, !(Group %in% c("23G-VA", "La Milpa N", "RBR04", "La Milpa Quarry", "24G-G", "24G-H", "24G-I", "24G-J", "24G-N", "24G-T", "RBR01", "RBR02", "RBR03")))

# Remove the ANID column from the dataset
laicpms_all_NE$ANID <- NULL

#split data into training (80%) and test set (20%)
set.seed(2025)

training.samples_laicpms_all_NE <- laicpms_all_NE$Group %>%
  createDataPartition(p = 0.8, list = FALSE)

train.data_laicpms_all_NE <- laicpms_all_NE[training.samples_laicpms_all_NE, ]
test.data_laicpms_all_NE <- laicpms_all_NE[-training.samples_laicpms_all_NE, ]

# Normalize the data 
# Estimate preprocessing parameters
preproc.param_laicpms_all_NE <- train.data_laicpms_all_NE%>% 
  preProcess(method = c("center", "scale"))

# Transform the data using the estimated parameters
train.transformed_laicpms_all_NE <- predict(preproc.param_laicpms_all_NE, train.data_laicpms_all_NE)
test.transformed_laicpms_all_NE <- predict(preproc.param_laicpms_all_NE, test.data_laicpms_all_NE)

# Check the structure of the transformed data to ensure it looks good
str(train.transformed_laicpms_all_NE)
str(test.transformed_laicpms_all_NE)

#Perform LDA
lda_laicpms_all_NE <- lda(Group ~ ., data = train.transformed_laicpms_all_NE)

# Make predictions
test.prediction_laicpms_all_NE <- predict(lda_laicpms_all_NE, test.transformed_laicpms_all_NE)


# Model accuracy
accuracy_laicpms_all_NE <- mean(test.prediction_laicpms_all_NE$class == test.transformed_laicpms_all_NE$Group)
print(paste("Model accuracy: ", accuracy_laicpms_all_NE))

# Predicted classes
head(test.prediction_laicpms_all_NE$class, 6)
# Predicted probabilities of class membership.
head(test.prediction_laicpms_all_NE$posterior, 6) 
# Linear discriminants
head(test.prediction_laicpms_all_NE$x, 3) 

lda.data_laicpms_all_NE <- cbind(train.transformed_laicpms_all_NE, predict(lda_laicpms_all_NE)$x)

ggplot(lda.data_laicpms_all_NE, aes(LD1, LD2)) +
  geom_point(alpha = 1, aes(color = Group)) +
  stat_ellipse(aes(color = Group)) +
  labs(title = "LDA of LAICPMS Data", x = "LDA Component 1", y = "LDA Component 2", color = "Source Groups", caption = "Feature importance = 59") +
  scale_color_manual(values = c("red", "blue", "green", "yellow", "orange", "purple", "pink", "brown", "gray", 
                                "black", "white", "cyan", "magenta", "lightblue", "darkgreen", "gold", 
                                "beige", "salmon", "peachpuff", "#E6E6FA", "darkblue", "turquoise")) +  # Hex code for lavender and dark blue
  annotate(
    "text", 
    x = Inf, y = -Inf, label = paste("LDA Model Accuracy:", round(accuracy_laicpms_all_NE, 10)), 
    hjust = 1.1, vjust = -0.5, size = 4, fontface = "italic", color = "black"
  )

#####################
#RIOBRAVO 

laicpms_all_NW <- subset(laicpms_all, !(Group %in% c("Rancho", "23G-AA", "23G-BA", "23G-F", "23G-E", "23G-I", "23G-V", "23G-W", "23G-X", "23G-Y", "23G-Z", "24G-J")))

View(laicpms_all_NW)
# Remove the ANID column from the dataset
laicpms_all_NW$ANID <- NULL

#split data into training (80%) and test set (20%)
set.seed(2025)


training.samples_laicpms_all_NW <- laicpms_all_NW$Group %>%
  createDataPartition(p = 0.8, list = FALSE)

train.data_laicpms_all_NW <- laicpms_all_NW[training.samples_laicpms_all_NW, ]
test.data_laicpms_all_NW <- laicpms_all_NW[-training.samples_laicpms_all_NW, ]

# Normalize the data 
# Estimate preprocessing parameters
preproc.param_laicpms_all_NW <- train.data_laicpms_all_NW%>% 
  preProcess(method = c("center", "scale"))

# Transform the data using the estimated parameters
train.transformed_laicpms_all_NW <- predict(preproc.param_laicpms_all_NW, train.data_laicpms_all_NW)
test.transformed_laicpms_all_NW <- predict(preproc.param_laicpms_all_NW, test.data_laicpms_all_NW)

# Check the structure of the transformed data to ensure it looks good
str(train.transformed_laicpms_all_NW)
str(test.transformed_laicpms_all_NW)

View(train.data_laicpms_all_NW)

#Perform LDA
lda_laicpms_all_NW <- lda(Group ~ ., data = train.transformed_laicpms_all_NW)

# Make predictions
test.prediction_laicpms_all_NW <- predict(lda_laicpms_all_NW, test.transformed_laicpms_all_NW)


# Model accuracy
accuracy_laicpms_all_NW <- mean(test.prediction_laicpms_all_NW$class == test.transformed_laicpms_all_NW$Group)
print(paste("Model accuracy: ", accuracy_laicpms_all_NW))

# Predicted classes
head(test.prediction_laicpms_all_NW$class, 6)
# Predicted probabilities of class membership.
head(test.prediction_laicpms_all_NW$posterior, 6) 
# Linear discriminants
head(test.prediction_laicpms_all_NW$x, 3) 

lda.data_laicpms_all_NW <- cbind(train.transformed_laicpms_all_NW, predict(lda_laicpms_all_NW)$x)

ggplot(lda.data_laicpms_all_NW, aes(LD1, LD2)) +
  geom_point(alpha = 1, aes(color = Group)) +
  stat_ellipse(aes(color = Group)) +
  labs(title = "LDA of LAICPMS Data", x = "LDA Component 1", y = "LDA Component 2", color = "Source Groups") +
  scale_color_manual(values = c("red", "blue", "green", "yellow", "orange", "purple", "pink", "brown", "gray", 
                                "black", "white", "cyan", "magenta", "lightblue", "darkgreen", "gold", 
                                "beige", "salmon", "peachpuff", "#E6E6FA", "darkblue", "turquoise")) +  # Hex code for lavender and dark blue
  annotate(
    "text", 
    x = Inf, y = -Inf, label = paste("LDA Model Accuracy:", round(accuracy_laicpms_all_NW, 10)), 
    hjust = 1.1, vjust = -0.5, size = 4, fontface = "italic", color = "black"
  )
