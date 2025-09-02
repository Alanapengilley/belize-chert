# Load necessary libraries
library(MASS)
library(ggplot2)
library(dplyr)
library(lattice)
library(caret)


# Load the geochemical dataset
df <- read_excel("LAICPMS Data/Geology LAICPMS.xlsx") 

#split data into training (80%) and test set (20%)
set.seed(123)

training.samplesdf <- df$Group %>%
  createDataPartition(p = 0.8, list = FALSE)

train.datadf <- df[training.samplesdf, ]
test.datadf <- df[-training.samplesdf, ]

# Normalize the data 
# Estimate preprocessing parameters
preproc.param <- train.datadf %>% 
  preProcess(method = c("center", "scale"))
# Transform the data using the estimated parameters
train.transformeddf <- preproc.param %>% predict(train.datadf)
test.transformeddf <- preproc.param %>% predict(test.datadf)

#remove non numeric columns 
train_numeric <- train.transformeddf[, sapply(train.transformeddf, is.numeric)]

#perform PCA to reduce dimensionality and handle collinearity 
pca_result <- prcomp(train_numeric, scale. = TRUE)

#Choose principal components (e.g., the first two)
pca_data <- pca_result$x[, 1:2] 

#Perform LDA on PCA-transformed data
lda_model <- lda(class ~ ., data = data.frame(class = train_class, pca_data))

# Fit the model
modeldf <- lda(Group~., data = train.transformeddf) 
modeldf

#plot(modeldf)

# Make predictions
test.predictiondf <- modeldf %>% predict(test.transformeddf)
names(test.predictiondf)

# Model accuracy
mean(test.predictiondf$class==test.transformeddf$Group)

# Predicted classes
head(test.predictiondf$class, 6)
# Predicted probabilities of class membership.
head(test.predictiondf$posterior, 6) 
# Linear discriminants
head(test.predictiondf$x, 3) 

lda.datadf <- cbind(train.transformeddf, predict(modeldf)$x)

ggplot(lda.datadf, aes(LD1, LD2)) +
  geom_point(aes(color = Group)) +
  stat_ellipse(aes(color = Group))


