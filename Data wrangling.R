#First load library needed
library(dplyr)
library(ggplot2)

#Create new df with the selected variables
BC <- dplyr:: select(breast_cancer, c(diagnosis,area_mean, smoothness_mean, compactness_mean, symmetry_mean))
BC

#Recode the categorical data to numerical data
BC$diagnosis[BC$diagnosis == "M"] <- "1"
BC$diagnosis[BC$diagnosis == "B"] <- "2"

breast_cancer$diagnosis[breast_cancer$diagnosis == "M"] <- "1"
breast_cancer$diagnosis[breast_cancer$diagnosis == "B"] <- "2"

#Modeling using stepwise regression
fitall = lm(diagnosis ~ ., data = BC)
formula(fitall)
summary(fitall)

fitstart = lm(diagnosis ~ 1, data = BC)
head(BC)

step(fitstart, direction = "both", scope = formula(fitall))


#Change name of column with spaces
names(breast_cancer)[names(breast_cancer) == "concave points_mean"]<- "concave_points_mean"

#Add two other variables to dataframe from origianl data frame
BC <- dplyr:: select(breast_cancer, c(diagnosis,area_mean, smoothness_mean, compactness_mean, symmetry_mean, concave_points_mean, concavity_mean))

#Linear regression of two of the variables
lin_reg <- lm(diagnosis ~ concave_points_mean, BC)
lin_reg1 <- lm(diagnosis ~ concavity_mean, BC)
print(lin_reg)
print(lin_reg1)
summary(lin_reg)
summary(lin_reg1)

#Both p-values are significant.  Adjusted R-square value is 60% for concave points mean and 48% for concavity mean.  It appears the concave points mean has a bigger impact.
#
###
#Logistic regression Libraries
library("caret")
library("magrittr")
library("dplyr")
library("tidyr")
library("lmtest")
library("popbio")
library("e1071")

#Begin Sarah's work#
#Change name of column with spaces
#I have the concave points variable as **concave.points_mean**

#Sarah's Smoothness Basic Logistic Model
summary(BC)
BC$diagnosisR <- NA
BC$diagnosisR[BC$diagnosis=='M'] <- 1
BC$diagnosisR[BC$diagnosis=='B'] <- 0
smoothlogit <- glm(diagnosis ~ smoothness_mean, data=BC, family="binomial")
#predict Diagnosis
probabilitiesmooth <- predict(smoothlogit, type = "response")
#Recode Predicted variable
BC$Predicted <- ifelse(probabilities > .5, "pos", "neg")
BC$PredictedR <- NA
BC$PredictedR[BC$Predicted =='pos'] <- 1
BC$PredictedR[BC$Predicted =='neg'] <- 0

#Convert Variables to Factors
BC$PredictedR <- as.factor(BC$PredictedR)
BC$diagnosisR <- as.factor(BC$diagnosisR)

#Create Confusion Matrix
conf_mat <- caret::confusionMatrix(BC$PredictedR, BC$diagnosisR)
conf_mat

#End Sarah's work#

















