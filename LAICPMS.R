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


#load in LAICPMS geological data
chert <- read_excel("Geology LAICPMS.xlsx") 
head(chert)
View(chert) #view df 




source_groups <- unique(chert$Group)
print(source_groups) #source groups 



#Add in ratios to initial dataframe 


chert$Fe_Ti <- chert$Fe2O3 / chert$Ti
chert$Ni_Co <- chert$Ni / chert$Co
chert$Mn_Fe <- chert$Mn / chert$Fe2O3
chert$Al_AlFe <- chert$Al2O3 / (chert$Al2O3 + chert$Fe2O3)
chert$Ca_Mg <- chert$CaCO3 / chert$MgO
chert$La_Ce <- chert$La / chert$Ce
chert$Ti_Al <- chert$Ti / chert$Al2O3
chert$Zr_Ti <- chert$Zr / chert$Ti
chert$Cr_Ni <- chert$Cr / chert$Ni
chert$Si_Al <- chert$SiO2 / chert$Al2O3
chert$Fe_Al <- chert$Fe2O3 / chert$Al2O3



#Create subsets of data 

# Remove Collinear values for LDA analysis 
df <- chert[c("Group", "SiO2", "Li", "Be", "B", "Na2O", "MgO", "Al2O3", "P", "K2O", "CaCO3", "Sc", "Ti", "V", "Cr", "Mn", "Fe2O3", "Ni", "Co", "Cu", "Zn", "Ga", "Ge", "Rb", "Sr", "Y", "Zr", "Nb", "Ba", "La", "Ce", "Pr", "Nd", "Pb", "U", "Fe_Ti", "Al_AlFe", "La_Ce", "Zr_Ti", "Cr_Ni", "Si_Al", "Fe_Al")]


#Create subsets of data
df_1 <- subset(df, !(Group %in% c("24G-J")))


NE_df <- subset(df, !(Group %in% c("23G-VA", "23G-XA", "24G-A", "24G-B", "24G-G", "24G-H", "24G-I", "24G-J", "24G-N", "24G-T", "RBR01", "RBR02", "RBR03")))

NW_df <- subset(df, !(Group %in% c("23G-A", "23G-AA", "23G-BA", "23G-F", "23G-E", "23G-I", "23G-V", "23G-W", "23G-X", "23G-Y", "23G-Z", "24G-J")))

RBR <- subset(df, !(Group %in% c("23G-A", "23G-AA", "23G-BA", "23G-F", "23G-E", "23G-I", "23G-V", "23G-W", "23G-X", "23G-Y", "23G-Z", "24G-J", "23G-VA", "23G-XA", "24G-B", "24G-G", "24G-H", "24G-I", "24G-J", "24G-N", "24G-T")))

Rivers <- subset(df, !(Group %in% c("23G-A", "23G-W", "23G-X", "23G-AA", "23G-BA", "23G-F", "23G-E", "23G-I", "23G-V", "23G-Y", "23G-Z", "24G-J", "23G-VA", "24G-B", "24G-G", "24G-H", "24G-I", "24G-J", "24G-N", "24G-T")))


Rivers <- subset(df, !(Group %in% c("23G-W", "23G-X", "23G-AA", "23G-BA", "23G-F", "23G-E", "23G-I", "23G-V", "23G-Y", "23G-Z", "24G-J", "23G-VA","23G-XA", "24G-B", "24G-G", "24G-H", "24G-I", "24G-J", "24G-N", "24G-T")))

# log10 transform 

df_log10 <- df_1
df_log10[] <- lapply(df_log10, function(x) if(is.numeric(x)) log10(x) else x)

NE_log10 <- NE_df
NE_log10[] <- lapply(NE_log10, function(x) if(is.numeric(x)) log10(x) else x)

NW_log10 <- NW_df
NW_log10[] <- lapply(NW_log10, function(x) if(is.numeric(x)) log10(x) else x)

RBR_log10 <- RBR
RBR_log10[] <- lapply(RBR_log10, function(x) if(is.numeric(x)) log10(x) else x)


#Ternary

#Al, Fe, Mm 


# Exploratory scatterplots

#Whole data frame 
ggplot(df_log10, aes(x=Ba, y=Mn, color=Group)) + geom_point()
ggplot(df_log10, aes(x=U, y=Ba, color=Group)) + geom_point()
ggplot(df_log10, aes(x=Cr, y=Na2O, color=Group)) + geom_point()
ggplot(df_log10, aes(x=SiO2, y=Ba, color=Group)) + geom_point()
ggplot(df_log10, aes(x=Ca_Mg, y=Zr_Ti, color=Group)) + geom_point()
ggplot(df_log10, aes(x=U, y=B, color=Group)) + geom_point()
ggplot(df_log10, aes(x=Al2O3, y=B, color=Group)) + geom_point()
ggplot(df_log10, aes(x=Cr, y=Zr, color=Group)) + geom_point()

#North Eastern subset 
ggplot(NE_log10, aes(x=Ba, y=Mn, color=Group)) + geom_point()
ggplot(NE_log10, aes(x=U, y=Ba, color=Group)) + geom_point()
ggplot(NE_log10, aes(x=Cr, y=Na2O, color=Group)) + geom_point()
ggplot(NE_log10, aes(x=SiO2, y=Ba, color=Group)) + geom_point()
ggplot(NE_log10, aes(x=Si_Al, y=Zr_Ti, color=Group)) + geom_point()
ggplot(NE_log10, aes(x=Ca_Mg, y=Zr_Ti, color=Group)) + geom_point()
ggplot(NE_log10, aes(x=U, y=B, color=Group)) + geom_point()
ggplot(NE_log10, aes(x=Al2O3, y=B, color=Group)) + geom_point()
ggplot(NE_log10, aes(x=B, y=Zr, color=Group)) + geom_point()

#North Western subset 
ggplot(NW_log10, aes(x=Si_Al, y=Zr_Ti, color=Group)) + geom_point()
ggplot(NW_log10, aes(x=Ba, y=MgO, color=Group)) + geom_point()
ggplot(NW_log10, aes(x=SiO2, y=Ba, color=Group)) + geom_point()
ggplot(NW_log10, aes(x=Cr, y=Na2O, color=Group)) + geom_point()
ggplot(NW_log10, aes(x=U, y=Ba, color=Group)) + geom_point()
ggplot(NW_log10, aes(x=Ba, y=Mn, color=Group)) + geom_point()
ggplot(NW_log10, aes(x=U, y=B, color=Group)) + geom_point()
ggplot(NW_log10, aes(x=Al2O3, y=B, color=Group)) + geom_point()
ggplot(NW_log10, aes(x=B, y=Zr, color=Group)) + geom_point()


#Rio Bravo River Sources 
ggplot(RBR_log10, aes(x=Al2O3, y=U, color=Group)) + geom_point()
ggplot(RBR_log10, aes(x=Al_AlFe, y=V, color=Group)) + geom_point()


# create ratios 




### PCA

#All Data 

chert_PCA <- chert[, 3:50]

data_normalized_all <-scale(chert_PCA) #normalize numerical data 
head(data_normalized_all)

data.pca_all <- princomp(data_normalized_all) #apply PCA
summary(data.pca_all) #PCA summary

data.pca_all$loadings[, 1:2] #loading matrix of the first two PCs

fviz_eig(data.pca_all, addlabel=TRUE) #Scree plot of the component 

fviz_pca_var(data.pca_all) #graph of variables

autoplot(data.pca_all,
         data = chert, 
         colour = "Group")


#North Western Belize 
NWPCA <- NW_df[, 2:42]

data_normalized_NW <-scale(NWPCA) #normalize numerical data 
head(data_normalized_NW)

class(data_normalized_NW)

data.pcaNW <- princomp(data_normalized_NW) #apply PCA
summary(data.pcaNW) #PCA summary

data.pcaNW$loadings[, 1:2] #loading matrix of the first two PCs

fviz_eig(data.pcaNW, addlabel=TRUE) #Scree plot of the component 

fviz_pca_var(data.pcaNW) #graph of variables

autoplot(data.pcaNW,
         data = NW_df, 
         colour = "Group")


#North Eastern Belize 
NEPCA <- NE_df[, 2:33]
print(NEPCA)

data_normalized_NE <-scale(NEPCA) #normalize numerical data 
head(data_normalized_NE)

class(data_normalized_NE)

data.pcaNE <- princomp(data_normalized_NE) #apply PCA
summary(data.pcaNE) #PCA summary

data.pcaNE$loadings[, 1:2] #loading matrix of the first two PCs

fviz_eig(data.pcaNE, addlabel=TRUE) #Scree plot of the component 

fviz_pca_var(data.pcaNE) #graph of variables

autoplot(data.pcaNE,
         data = NE_df, 
         colour = "Group")



#Rio Bravo Sources
RBRPCA <- RBR[, 2:42]
print(RBRPCA)

data_normalized_RBR <-scale(RBRPCA) #normalize numerical data 
head(data_normalized_RBR)

class(data_normalized_RBR)

data.pcaRBR <- princomp(data_normalized_RBR) #apply PCA
summary(data.pcaRBR) #PCA summary

data.pcaRBR$loadings[, 1:2] #loading matrix of the first two PCs

fviz_eig(data.pcaRBR, addlabel=TRUE) #Scree plot of the component 

fviz_pca_var(data.pcaRBR) #graph of variables

autoplot(data.pcaRBR,
         data = RBR, 
         colour = "Group")




### Discriminate


## Check for Collinearity among variables 

#library(caret)

# Find highly correlated features
#correlation_matrix <- cor(chert[, -c(1:2)])  # Exclude descriptive variable
#high_corr_vars <- findCorrelation(correlation_matrix, cutoff = 0.9)
#print(high_corr_vars) #variables 37 and 7 are collinear 


#print the 37 and 7 variables in dataset 
#column37 <- colnames(Chert)[37, 7]
#print(column37) #Cs

#column7 <- colnames(Chert)[7]
#print(column7) #MgO




## All data (24G-J removed- dolomitic limestone)


df_DA<- df_1[, 1:35] #remove ratios 


#split data into training (80%) and test set (20%)
set.seed(123)

training.samplesdf <- df_DA$Group %>%
  createDataPartition(p = 0.8, list = FALSE)

train.datadf <- df_DA[training.samplesdf, ]
test.datadf <- df_DA[-training.samplesdf, ]

print(train.datadf)
print(test.datadf)


# Normalize the data 
# Estimate preprocessing parameters
preproc.param <- train.datadf %>% 
  preProcess(method = c("center", "scale"))
# Transform the data using the estimated parameters
train.transformeddf <- preproc.param %>% predict(train.datadf)
test.transformeddf <- preproc.param %>% predict(test.datadf)


print(test.transformed2)

class(train.transformed2)
class(test.transformed2)


library(MASS)
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





## North Western Belize 

NWdf_1<- NW_df[, 1:35] #remove ratios 

view(NWdf_1)

#split data into training (80%) and test set (20%)
set.seed(123)

training.samplesNW <- NWdf_1$Group %>%
  createDataPartition(p = 0.8, list = FALSE)

train.dataNW <- NWdf_1[training.samplesNW, ]
test.dataNW <- NWdf_1[-training.samplesNW, ]

print(train.dataNW)
print(test.dataNW)


# Normalize the data 
# Estimate preprocessing parameters
preproc.param <- train.dataNW %>% 
  preProcess(method = c("center", "scale"))
# Transform the data using the estimated parameters
train.transformedNW <- preproc.param %>% predict(train.dataNW)
test.transformedNW <- preproc.param %>% predict(test.dataNW)


library(MASS)
# Fit the model
modelNW <- lda(Group~., data = train.transformedNW) 
modelNW

#plot(model2)


# Make predictions
test.predictionsNW <- modelNW %>% predict(test.transformedNW)
names(test.predictionSNW)

# Model accuracy
mean(test.predictionsNW$class==test.transformedNW$Group)

# Predicted classes
head(test.predictionSNW$class, 6)
# Predicted probabilities of class membership.
head(test.predictionSNW$posterior, 6) 
# Linear discriminants
head(test.predictionSNW$x, 3) 

lda.dataNW <- cbind(train.transformedNW, predict(modelNW)$x)

ggplot(lda.dataNW, aes(LD1, LD2)) +
  geom_point(aes(color = Group)) +
  stat_ellipse(aes(color = Group))



## North Eastern Belize 

NEdf_1 <- NE_df[, 1:35] #remove ratios 

#split data into training (80%) and test set (20%)
set.seed(123)

training.samplesNE <- NEdf_1$Group %>%
  createDataPartition(p = 0.8, list = FALSE)

train.dataNE <- NEdf_1[training.samplesNE, ]
test.dataNE <- NEdf_1[-training.samplesNE, ]

print(train.dataNE)
print(test.dataNE)


# Normalize the data 
# Estimate preprocessing parameters
preproc.param <- train.dataNE %>% 
  preProcess(method = c("center", "scale"))
# Transform the data using the estimated parameters
train.transformedNE <- preproc.param %>% predict(train.dataNE)
test.transformedNE <- preproc.param %>% predict(test.dataNE)


print(test.transformedNE)

class(train.transformedNE)
class(test.transformedNE)


library(MASS)
# Fit the model
model_NE <- lda(Group~., data = train.transformedNE) 
model_NE

# Make predictions
test.predictionsNE <- model_NE %>% predict(test.transformedNE)
names(test.predictionsNE)

# Model accuracy
mean(test.predictionsNE$class==test.transformedNE$Group)

# Predicted classes
head(test.predictionsNE$class, 6)
# Predicted probabilities of class membership.
head(test.predictionsNE$posterior, 6) 
# Linear discriminants
head(test.predictionsNE$x, 3) 

lda.data_NE <- cbind(train.transformedNE, predict(model_NE)$x)

ggplot(lda.data_NE, aes(LD1, LD2)) +
  geom_point(aes(color = Group)) +
  stat_ellipse(aes(color = Group))



## Rio Bravo Sources

#split data into training (80%) and test set (20%)
set.seed(444)

training.samplesRBR <- RBR$Group %>%
  createDataPartition(p = 0.8, list = FALSE)

train.dataRBR <- RBR[training.samplesRBR, ]
test.dataRBR <- RBR[-training.samplesRBR, ]



# Normalize the data 
# Estimate preprocessing parameters
preproc.param <- train.dataRBR %>% 
  preProcess(method = c("center", "scale"))
# Transform the data using the estimated parameters
train.transformedRBR <- preproc.param %>% predict(train.dataRBR)
test.transformedRBR <- preproc.param %>% predict(test.dataRBR)


library(MASS)

# Fit the model
model_RBR <- lda(Group~., data = train.transformedRBR) 
model_RBR



# Make predictions
test.predictionsRBR <- model_RBR %>% predict(test.transformedRBR)
names(test.predictionsRBR)

# Model accuracy
mean(test.predictionsRBR$class==test.transformedRBR$Group)

# Predicted classes
head(test.predictionsRBR$class, 6)
# Predicted probabilities of class membership.
head(test.predictionsRBR$posterior, 6) 
# Linear discriminants
head(test.predictionsRBR$x, 3) 

lda.data_RBR <- cbind(train.transformedRBR, predict(model_RBR)$x)

ggplot(lda.data_RBR, aes(LD1, LD2)) +
  geom_point(aes(color = Group)) +
  stat_ellipse(aes(color = Group))


## Rio Bravo Rivers, La Milpa North 

Rivers_lda <- Rivers[, 1:35]
view(Rivers_lda)

#split data into training (80%) and test set (20%)
set.seed(444)

training.samplesRivers <- Rivers_lda$Group %>%
  createDataPartition(p = 0.8, list = FALSE)

train.dataRivers <- Rivers_lda[training.samplesRivers, ]
test.dataRivers <- Rivers_lda[-training.samplesRivers, ]

# Normalize the data 
# Estimate preprocessing parameters
preproc.param <- train.dataRivers %>% 
  preProcess(method = c("center", "scale"))
# Transform the data using the estimated parameters
train.transformedRivers <- preproc.param %>% predict(train.dataRivers)
test.transformedRivers <- preproc.param %>% predict(test.dataRivers)

library(MASS)

# Fit the model
model_Rivers <- lda(Group~., data = train.transformedRivers) 
model_Rivers

# Make predictions
test.predictionsRivers <- model_Rivers %>% predict(test.transformedRivers)
names(test.predictionsRivers)

# Model accuracy
mean(test.predictionsRivers$class==test.transformedRivers$Group)

# Predicted classes
predicted_classes <- test.predictionsRivers$class
# Predicted probabilities of class membership.
head(test.predictionsRivers$posterior, 6) 
# Linear discriminants
head(test.predictionsRivers$x, 3) 


#Plot Predicted vs. True values 
lda_values <- test.predictionsRivers$x
plot_data <- data.frame(lda_values, Actual = test.dataRivers$Group, Predicted = predicted_classes)

ggplot(plot_data, aes(x = LD1, y = LD2, color = Predicted)) +
  geom_point(size = 2) +
  labs(title = "LDA: Predicted vs True Classes",
       x = "Linear Discriminant 1 (LD1)",
       y = "Linear Discriminant 2 (LD2)") 

lda.data_Rivers <- cbind(train.transformedRivers, predict(model_Rivers)$x)

ggplot(lda.data_Rivers, aes(LD1, LD2)) +
  geom_point(aes(color = Group)) +
  stat_ellipse(aes(color = Group))






## Rio Bravo Rivers, and Rancho Creek 

Rivers_lda <- Rivers[, 1:35]
view(Rivers_lda)

#split data into training (80%) and test set (20%)
set.seed(444)

training.samplesRivers <- Rivers_lda$Group %>%
  createDataPartition(p = 0.8, list = FALSE)

train.dataRivers <- Rivers_lda[training.samplesRivers, ]
test.dataRivers <- Rivers_lda[-training.samplesRivers, ]

# Normalize the data 
# Estimate preprocessing parameters
preproc.param <- train.dataRivers %>% 
  preProcess(method = c("center", "scale"))
# Transform the data using the estimated parameters
train.transformedRivers <- preproc.param %>% predict(train.dataRivers)
test.transformedRivers <- preproc.param %>% predict(test.dataRivers)

library(MASS)

# Fit the model
model_Rivers <- lda(Group~., data = train.transformedRivers) 
model_Rivers

# Make predictions
test.predictionsRivers <- model_Rivers %>% predict(test.transformedRivers)
names(test.predictionsRivers)

# Model accuracy
mean(test.predictionsRivers$class==test.transformedRivers$Group)

# Predicted classes
predicted_classes <- test.predictionsRivers$class
# Predicted probabilities of class membership.
head(test.predictionsRivers$posterior, 6) 
# Linear discriminants
head(test.predictionsRivers$x, 3) 


#Plot Predicted vs. True values 
lda_values <- test.predictionsRivers$x
plot_data <- data.frame(lda_values, Actual = test.dataRivers$Group, Predicted = predicted_classes)

ggplot(plot_data, aes(x = LD1, y = LD2, color = Predicted)) +
  geom_point(size = 2) +
  labs(title = "LDA: Predicted vs True Classes",
       x = "Linear Discriminant 1 (LD1)",
       y = "Linear Discriminant 2 (LD2)") 

lda.data_Rivers <- cbind(train.transformedRivers, predict(model_Rivers)$x)

ggplot(lda.data_Rivers, aes(LD1, LD2)) +
  geom_point(aes(color = Group)) +
  stat_ellipse(aes(color = Group))




### Artifacts 

#load in LAICPMS Artifact data
artifacts <- read_excel("Artifacts.xlsx") 
head(artifacts)
View(artifacts) #view df 

sites <- unique(artifacts$Site)
print(sites) #source groups 


artifacts_log10 <- artifacts
artifacts_log10[] <- lapply(artifacts_log10, function(x) if(is.numeric(x)) log10(x) else x)


# Exploratory scatterplots

#Whole data frame 
ggplot(artifacts_log10, aes(x=Ba, y=U, color=Site)) + geom_point()
ggplot(artifacts_log10, aes(x=B, y=U, color=Site)) + geom_point()
ggplot(artifacts_log10, aes(x=B, y=Al2O3, color=Site)) + geom_point()
ggplot(artifacts_log10, aes(x=B, y=Sr, color=Site)) + geom_point()

#PCA
artifact_PCA <- artifacts[, 3:61]
print(artifact_PCA)

data_normalized_artifacts <-scale(artifact_PCA) #normalize numerical data 
head(data_normalized_artifacts)

class(data_normalized_artifacts)

data.pca_artifacts <- princomp(data_normalized_artifacts) #apply PCA
summary(data.pca_artifacts) #PCA summary

data.pca_artifacts$loadings[, 1:2] #loading matrix of the first two PCs

fviz_eig(data.pca_artifacts, addlabel=TRUE) #Scree plot of the component 

fviz_pca_var(data.pca_artifacts) #graph of variables

autoplot(data.pca_artifacts,
         data = artifacts, 
         colour = "Site")


# LDA

# Normalize the data 
# Estimate preprocessing parameters
preproc.param <-  artifacts %>% 
  preProcess(method = c("center", "scale"))
# Transform the data using the estimated parameters
artifacts.transformed <- preproc.param %>% predict(artifacts)

#make prediciations
artifacts.transformed_Predict <- model_Rivers %>% predict(artifacts.transformed)
names(artifacts.transformed_Predict)


# Predicted classes
artifact_classpredictions <- artifacts.transformed_Predict$class
# Predicted probabilities of class membership.
head(artifacts.transformed_Predict$posterior, 10) 
# Linear discriminants
head(artifacts.transformed_Predict$x, 10) 

# Combine the predicted classes with the test data (if you want to save or inspect them)
predicted_artifacts <- cbind(artifacts.transformed, Predicted_Class = artifact_classpredictions)
# View the predicted data
head(predicted_artifacts)

# Save the predictions to a CSV file (optional)
write.csv(predicted_artifacts, "predicted_groups.csv", row.names = FALSE)

#visualize predictions (not very useful)
lda_values_artifacts <- artifacts.transformed_Predict$x  # Scores on the linear discriminants
plot_data_artifacts <- data.frame(lda_values_artifacts, Predicted_Class = artifact_classpredictions)

# Plot the first two linear discriminants
ggplot(plot_data_artifacts , aes(x = LD1, y = LD2, color = Predicted_Class)) +
  geom_point(size = 2) +
  labs(title = "LDA: Predicted Group Classification",
       x = "Linear Discriminant 1 (LD1)",
       y = "Linear Discriminant 2 (LD2)") +
  theme_minimal()


#Combine the training and unknown data for plotting 
# Get the LDA scores (the linear discriminants) for both training and test data
lda_train_values <- predict(model_Rivers, newdata = train.transformedRivers)$x  # LDA scores for training data
lda_values_artifacts <- artifacts.transformed_Predict$x   # LDA scores for unknown data

# Combine training data with the predicted classes and test data
plot_data <- rbind(
  data.frame(lda_train_values, Class = train.transformedRivers$Group, Type = "Training"),
  data.frame(lda_values_artifacts, Class = artifact_classpredictions, Type = "Predicted")
)

# View the combined data for the plot
head(plot_data)

view(plot_data)

ggplot(plot_data, aes(x = LD3, y = LD1, color = Class, shape = Type)) +
  geom_point(size = 2) +
  labs(title = "LDA: Training Data vs Predicted Classes",
       x = "Linear Discriminant 1 (LD3)",
       y = "Linear Discriminant 2 (LD1)") +
  scale_shape_manual(values = c(16, 17)) +  # Different shapes for training vs predicted
  theme(legend.position = "bottom")







