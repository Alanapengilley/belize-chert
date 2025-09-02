install.packages("tidyverse")
install.packages("ggplot2")
install.packages("corrr")
install.packages("ggcorrplot")
install.packages("FactoMineR")
install.packages("devtools")
install_github("kassambara/factoextra")
install.packages("caret")
install.packages("dplyr")

#load in packages 
library(tidyverse)
library(readxl)
library(ggplot2)
library(lattice)
library(corrr)
library(ggcorrplot)
library(FactoMineR)
library(devtools)
library(factoextra)
library(ggfortify)
library(caret)
library(dplyr)


#load in XRF dataframe 
xrf <- read_excel("XRF.xlsx") 
head(xrf)
View(xrf) #view df 


# exploratory scatterplot 
ggplot(xrf, aes(x=Fe2O3, y=Y, color=Group)) + geom_point()


ggplot(xrf, aes(x=Y, y=Zr, color=Group)) + geom_point()

# PCA

data <- xrf[, c("Group", "Na2O", "Al2O3", "SiO2", "K2O", "CaO", "Fe2O3", "Rb", "Y", "Zr", "Nb", "Pb","Th")]

print(data)


data_normalized <-scale(data) #normalize numerical data 
head(data_normalized)

data.pca <- princomp(data_normalized) #apply PCA
summary(data.pca) #PCA summary

data.pca$loadings[, 1:2] #loading matrix of the first two PCs

fviz_eig(data.pca, addlabel=TRUE) #Scree plot of the component 

fviz_pca_var(data.pca) #graph of variables

autoplot(data.pca,
         data = xrf, 
         colour = "Group")



###

data1 <- xrf[, c( "Rb", "Y", "Zr", "Nb", "Pb","Th")]

data_normalized1 <-scale(data1) #normalize numerical data 
head(data_normalized)

data.pca1 <- princomp(data_normalized1) #apply PCA
summary(data.pca1) #PCA summary

data.pca1$loadings[, 1:2] #loading matrix of the first two PCs

fviz_eig(data.pca1, addlabel=TRUE) #Scree plot of the component 

fviz_pca_var(data.pca1) #graph of variables

autoplot(data.pca1,
         data = xrf, 
         colour = "Group")


#Discriminate

print(data_normalized) #use normalized data 

#split data into training (80%) and test set (20%)
set.seed(123)

training.samples <- createDataPartition(data$Group, list = FALSE)
train.data <- data[training.samples, ]+
test.data <- data[-training.samples, ]

class(train.data)

# Estimate preprocessing parameters
preproc.param <- train.data %>% 
  preProcess(method = c("center", "scale"))
# Transform the data using the estimated parameters
train.transformed <- preproc.param %>% predict(train.data)
test.transformed <- preproc.param %>% predict(test.data)

class(train.transformed)

library(MASS)
# Fit the model
model <- lda(Group~., data = train.transformed)
model

# Make predictions
predictions <- model %>% predict(test.transformed)
names(predictions)

# Model accuracy
mean(predictions$class==test.transformed$Group)

# Predicted classes
head(predictions$class, 6)
# Predicted probabilities of class memebership.
head(predictions$posterior, 6) 
# Linear discriminants
head(predictions$x, 3) 

lda.data <- cbind(train.transformed, predict(model)$x)
ggplot(lda.data, aes(LD1, LD2)) +
  geom_point(aes(color = Group)) +
  stat_ellipse(aes(color = Group))
