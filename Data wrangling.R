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


















