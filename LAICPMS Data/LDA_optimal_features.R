#LAICPMS DATASETS WITH OPTIMAL FEATURES SELECTED BY RFECV 
#CURRENT NUMBER = 34

#LDA 

#LOAD IN DATA
laicpms_optimal <- read.csv("LAICPMS Data/subset_data_optimal_features.csv")

colnames(laicpms_optimal)

###############
#ALL material 

laicpms_optimal_all <- subset(laicpms_optimal, !(Group %in% c("24G-J")))

# Remove the ANID column from the dataset
laicpms_optimal_all$ANID <- NULL

#split data into training (80%) and test set (20%)
set.seed(2025)

training.samples_laicpms_optimal_all <- laicpms_optimal_all$Group %>%
  createDataPartition(p = 0.8, list = FALSE)

train.data_laicpms_optimal_all <- laicpms_optimal_all[training.samples_laicpms_optimal_all, ]
test.data_laicpms_optimal_all <- laicpms_optimal_all[-training.samples_laicpms_optimal_all, ]

# Normalize the data 
# Estimate preprocessing parameters
preproc.param_laicpms_optimal_all <- train.data_laicpms_optimal_all %>% 
  preProcess(method = c("center", "scale"))

# Transform the data using the estimated parameters
train.transformed_laicpms_optimal_all <- predict(preproc.param_laicpms_optimal_all, train.data_laicpms_optimal_all)
test.transformed_laicpms_optimal_all <- predict(preproc.param_laicpms_optimal_all, test.data_laicpms_optimal_all)

# Check the structure of the transformed data to ensure it looks good
str(train.transformed_laicpms_optimal_all)
str(test.transformed_laicpms_optimal_all)

#Perform LDA
lda_laicpms_optimal_all <- lda(Group ~ ., data = train.transformed_laicpms_optimal_all)

# Make predictions
test.prediction_laicpms_optimal_all <- predict(lda_laicpms_optimal_all, test.transformed_laicpms_optimal_all)


# Model accuracy
accuracy_laicpms_optimal_all <- mean(test.prediction_laicpms_optimal_all$class == test.transformed_laicpms_optimal_all$Group)
print(paste("Model accuracy: ", accuracy_laicpms_optimal_all))

# Predicted classes
head(test.prediction_laicpms_optimal_all$class, 6)
# Predicted probabilities of class membership.
head(test.prediction_laicpms_optimal_all$posterior, 6) 
# Linear discriminants
head(test.prediction_laicpms_optimal_all$x, 3) 

lda.data_laicpms_optimal_all <- cbind(train.transformed_laicpms_optimal_all, predict(lda_laicpms_optimal_all)$x)

ggplot(lda.data_laicpms_optimal_all, aes(LD1, LD2)) +
  geom_point(alpha = 1, aes(color = Group)) +
  stat_ellipse(aes(color = Group)) +
  labs(title = "LDA of LAICPMS Data", x = "LDA Component 1", y = "LDA Component 2", color = "Source Groups", caption = "Feature importance = 34") +
  scale_color_manual(values = c("red", "blue", "green", "yellow", "orange", "purple", "pink", "brown", "gray", 
                                "black", "white", "cyan", "magenta", "lightblue", "darkgreen", "gold", 
                                "beige", "salmon", "peachpuff", "#E6E6FA", "darkblue", "turquoise")) +  # Hex code for lavender and dark blue
  annotate(
    "text", 
    x = Inf, y = -Inf, label = paste("LDA Model Accuracy:", round(accuracy_laicpms_optimal_all, 10)), 
    hjust = 1.1, vjust = -0.5, size = 4, fontface = "italic", color = "black"
  )


##############
#Only CBZ material 
laicpms_optimal_NE <- subset(laicpms_optimal, !(Group %in% c("23G-VA", "La Milpa N", "RBR04", "La Milpa Quarry", "24G-G", "24G-H", "24G-I", "24G-J", "24G-N", "24G-T", "RBR01", "RBR02", "RBR03")))

# Remove the ANID column from the dataset
laicpms_optimal_NE$ANID <- NULL

#split data into training (80%) and test set (20%)
set.seed(2025)

training.samples_laicpms_optimal_NE <- laicpms_optimal_NE$Group %>%
  createDataPartition(p = 0.8, list = FALSE)

train.data_laicpms_optimal_NE <- laicpms_optimal_NE[training.samples_laicpms_optimal_NE, ]
test.data_laicpms_optimal_NE <- laicpms_optimal_NE[-training.samples_laicpms_optimal_NE, ]

# Normalize the data 
# Estimate preprocessing parameters
preproc.param_laicpms_optimal_NE <- train.data_laicpms_optimal_NE%>% 
  preProcess(method = c("center", "scale"))

# Transform the data using the estimated parameters
train.transformed_laicpms_optimal_NE <- predict(preproc.param_laicpms_optimal_NE, train.data_laicpms_optimal_NE)
test.transformed_laicpms_optimal_NE <- predict(preproc.param_laicpms_optimal_NE, test.data_laicpms_optimal_NE)

# Check the structure of the transformed data to ensure it looks good
str(train.transformed_laicpms_optimal_NE)
str(test.transformed_laicpms_optimal_NE)

#Perform LDA
lda_laicpms_optimal_NE <- lda(Group ~ ., data = train.transformed_laicpms_optimal_NE)

# Make predictions
test.prediction_laicpms_optimal_NE <- predict(lda_laicpms_optimal_NE, test.transformed_laicpms_optimal_NE)


# Model accuracy
accuracy_laicpms_optimal_NE <- mean(test.prediction_laicpms_optimal_NE$class == test.transformed_laicpms_optimal_NE$Group)
print(paste("Model accuracy: ", accuracy_laicpms_optimal_NE))

# Predicted classes
head(test.prediction_laicpms_optimal_NE$class, 6)
# Predicted probabilities of class membership.
head(test.prediction_laicpms_optimal_NE$posterior, 6) 
# Linear discriminants
head(test.prediction_laicpms_optimal_NE$x, 3) 

lda.data_laicpms_optimal_NE <- cbind(train.transformed_laicpms_optimal_NE, predict(lda_laicpms_optimal_NE)$x)

ggplot(lda.data_laicpms_optimal_NE, aes(LD1, LD2)) +
  geom_point(alpha = 1, aes(color = Group)) +
  stat_ellipse(aes(color = Group)) +
  labs(title = "LDA of LAICPMS Data", x = "LDA Component 1", y = "LDA Component 2", color = "Source Groups", caption = "Feature importance = 34") +
  scale_color_manual(values = c("red", "blue", "green", "yellow", "orange", "purple", "pink", "brown", "gray", 
                                "black", "white", "cyan", "magenta", "lightblue", "darkgreen", "gold", 
                                "beige", "salmon", "peachpuff", "#E6E6FA", "darkblue", "turquoise")) +  # Hex code for lavender and dark blue
  annotate(
    "text", 
    x = Inf, y = -Inf, label = paste("LDA Model Accuracy:", round(accuracy_laicpms_optimal_NE, 10)), 
    hjust = 1.1, vjust = -0.5, size = 4, fontface = "italic", color = "black"
  )

#####################
#RIOBRAVO 

laicpms_optimal_NW <- subset(laicpms_optimal, !(Group %in% c("Rancho", "23G-AA", "23G-BA", "23G-F", "23G-E", "23G-I", "23G-V", "23G-W", "23G-X", "23G-Y", "23G-Z", "24G-J")))

View(laicpms_optimal_NW)
# Remove the ANID column from the dataset
laicpms_optimal_NW$ANID <- NULL

#split data into training (80%) and test set (20%)
set.seed(2025)


training.samples_laicpms_optimal_NW <- laicpms_optimal_NW$Group %>%
  createDataPartition(p = 0.8, list = FALSE)

train.data_laicpms_optimal_NW <- laicpms_optimal_NW[training.samples_laicpms_optimal_NW, ]
test.data_laicpms_optimal_NW <- laicpms_optimal_NW[-training.samples_laicpms_optimal_NW, ]

# Normalize the data 
# Estimate preprocessing parameters
preproc.param_laicpms_optimal_NW <- train.data_laicpms_optimal_NW%>% 
  preProcess(method = c("center", "scale"))

# Transform the data using the estimated parameters
train.transformed_laicpms_optimal_NW <- predict(preproc.param_laicpms_optimal_NW, train.data_laicpms_optimal_NW)
test.transformed_laicpms_optimal_NW <- predict(preproc.param_laicpms_optimal_NW, test.data_laicpms_optimal_NW)

# Check the structure of the transformed data to ensure it looks good
str(train.transformed_laicpms_optimal_NW)
str(test.transformed_laicpms_optimal_NW)

View(train.data_laicpms_optimal_NW)

#Perform LDA
lda_laicpms_optimal_NW <- lda(Group ~ ., data = train.transformed_laicpms_optimal_NW)

# Make predictions
test.prediction_laicpms_optimal_NW <- predict(lda_laicpms_optimal_NW, test.transformed_laicpms_optimal_NW)


# Model accuracy
accuracy_laicpms_optimal_NW <- mean(test.prediction_laicpms_optimal_NW$class == test.transformed_laicpms_optimal_NW$Group)
print(paste("Model accuracy: ", accuracy_laicpms_optimal_NW))

# Predicted classes
head(test.prediction_laicpms_optimal_NW$class, 6)
# Predicted probabilities of class membership.
head(test.prediction_laicpms_optimal_NW$posterior, 6) 
# Linear discriminants
head(test.prediction_laicpms_optimal_NW$x, 3) 

lda.data_laicpms_optimal_NW <- cbind(train.transformed_laicpms_optimal_NW, predict(lda_laicpms_optimal_NW)$x)

ggplot(lda.data_laicpms_optimal_NW, aes(LD1, LD2)) +
  geom_point(alpha = 1, aes(color = Group)) +
  stat_ellipse(aes(color = Group)) +
  labs(title = "LDA of LAICPMS Data from Rio Bravo", x = "LDA Component 1", y = "LDA Component 2", color = "Source Groups") +
  scale_color_manual(values = c("red", "blue", "green", "yellow", "orange", "purple", "pink", "brown", "gray", 
                                "black", "white", "cyan", "magenta", "lightblue", "darkgreen", "gold", 
                                "beige", "salmon", "peachpuff", "#E6E6FA", "darkblue", "turquoise")) +  # Hex code for lavender and dark blue
  annotate(
    "text", 
    x = Inf, y = -Inf, label = paste("LDA Model Accuracy:", round(accuracy_laicpms_optimal_NW, 10)), 
    hjust = 1.1, vjust = -0.5, size = 4, fontface = "italic", color = "black"
  )

###################
#STREAM SOURCES ONLY 

laicpms_optimal_streams <- subset(laicpms_optimal, !(Group %in% c("23G-W", "23G-X", "23G-AA", "23G-BA", "23G-F", "La Milpa Quarry", "23G-E", "23G-I", "23G-V", "23G-Y", "23G-Z", "24G-J", "23G-VA","La Milpa N", "24G-G", "24G-H", "24G-I", "24G-J", "24G-N", "24G-T")))

# Remove the ANID column from the dataset
laicpms_optimal_streams$ANID <- NULL

#split data into training (80%) and test set (20%)
set.seed(2025)


training.samples_laicpms_optimal_streams<- laicpms_optimal_streams$Group %>%
  createDataPartition(p = 0.8, list = FALSE)

train.data_laicpms_optimal_streams <- laicpms_optimal_streams[training.samples_laicpms_optimal_streams, ]
test.data_laicpms_optimal_streams <- laicpms_optimal_streams[-training.samples_laicpms_optimal_streams, ]

# Normalize the data 
# Estimate preprocessing parameters
preproc.param_laicpms_optimal_streams <- train.data_laicpms_optimal_streams%>% 
  preProcess(method = c("center", "scale"))

# Transform the data using the estimated parameters
train.transformed_laicpms_optimal_streams <- predict(preproc.param_laicpms_optimal_streams, train.data_laicpms_optimal_streams)
test.transformed_laicpms_optimal_streams <- predict(preproc.param_laicpms_optimal_streams, test.data_laicpms_optimal_streams)

# Check the structure of the transformed data to ensure it looks good
str(train.transformed_laicpms_optimal_streams)
str(test.transformed_laicpms_optimal_streams)


#Perform LDA
lda_laicpms_optimal_streams <- lda(Group ~ ., data = train.transformed_laicpms_optimal_streams)

# Make predictions
test.prediction_laicpms_optimal_streams <- predict(lda_laicpms_optimal_streams, test.transformed_laicpms_optimal_streams)


# Model accuracy
accuracy_laicpms_optimal_streams <- mean(test.prediction_laicpms_optimal_streams$class == test.transformed_laicpms_optimal_streams$Group)
print(paste("Model accuracy: ", accuracy_laicpms_optimal_streams))

# Predicted classes
head(test.prediction_laicpms_optimal_streams$class, 6)
# Predicted probabilities of class membership.
head(test.prediction_laicpms_optimal_streams$posterior, 6) 
# Linear discriminants
head(test.prediction_laicpms_optimal_streams$x, 3) 

lda.data_laicpms_optimal_streams <- cbind(train.transformed_laicpms_optimal_streams, predict(lda_laicpms_optimal_streams)$x)

ggplot(lda.data_laicpms_optimal_streams, aes(LD1, LD2)) +
  geom_point(alpha = 1, aes(color = Group)) +
  stat_ellipse(aes(color = Group)) +
  labs(title = "LDA of LAICPMS Data (stream sources)", x = "LDA Component 1", y = "LDA Component 2", color = "Source Groups",  caption = "Feature importance = 34") +
  scale_color_manual(values = c("red", "blue", "green", "yellow", "orange", "purple", "pink", "brown", "gray", 
                                "black", "white", "cyan", "magenta", "lightblue", "darkgreen", "gold", 
                                "beige", "salmon", "peachpuff", "#E6E6FA", "darkblue", "turquoise")) +  # Hex code for lavender and dark blue
  annotate(
    "text", 
    x = Inf, y = -Inf, label = paste("LDA Model Accuracy:", round(accuracy_laicpms_optimal_streams, 10)), 
    hjust = 1.1, vjust = -0.5, size = 4, fontface = "italic", color = "black"
  )
















