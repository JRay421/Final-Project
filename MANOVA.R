#Load Libraries
library("mvnormtest")
library("car")
library(dplyr)

#Load Data
breast.cancer <- read.csv("C:/Users/Student/Desktop/Group Project/archive/breast-cancer.csv")
View(breast.cancer)

#Create dataset with needed variable
bC <- select(breast.cancer, c(diagnosis, area_mean, smoothness_mean, compactness_mean, symmetry_mean, concavity_mean, concave.points_mean))

#Recode diagnosis
bC$diagnosisR <- NA
bC$diagnosisR[bC$diagnosis=='B'] <- 0
bC$diagnosisR[bC$diagnosis=='M'] <- 1

#Drop diagnosis 
bC1 <- select(bC, c(diagnosisR, area_mean, smoothness_mean, compactness_mean, symmetry_mean, concavity_mean, concave.points_mean))

#Independent variable (diagnosis)

#Check to make sure all variables are Numeric
str(bC1$area_mean)
str(bC1$smoothness_mean)
str(bC1$compactness_mean)
str(bC1$symmetry_mean)
str(bC1$concavity_mean)
str(bC1$concave.points_mean)

#Format as a matrix
bC1 <- as.matrix(bC)

#Test Assumptions
#Sample size is met 

#Multivariate Normality
mshapiro.test(t(bC1))
#The p value is less than 0.05 and is therefore significant
#The assumption has been violated

#Test for Homogeneity of Variance
leveneTest(area_mean ~ diagnosis, data=bC) #The p value is less than 0.05
leveneTest(smoothness_mean ~ diagnosis, data=bC) #The p value is greater than 0.05
leveneTest(compactness_mean ~ diagnosis, data=bC) #The p value is less than 0.05
leveneTest(symmetry_mean ~ diagnosis, data=bC) #The p value is greater than 0.05
leveneTest(concavity_mean ~ diagnosis, data=bC) #The p value is less than 0.05
leveneTest(concave.points_mean ~ diagnosis, data=bC) #The p value is less than 0.05
#ONly 2 variables meet the assumtion and the other 4 have failed

#Absence of Multicollinearity
cor.test(bC$area_mean, bC$smoothness_mean, method="pearson", use="complete.obs")
#There is absence of multicollinearity

cor.test(bC$compactness_mean, bC$symmetry_mean, method="pearson", use="complete.obs")
#There is absence of multicollinearity

cor.test(bC$concavity_mean, bC$concave.points_mean, method="pearson", use="complete.obs")
#There is not absence of mulicollinearty. There is strong correlation being over .7

#Analysis
MANOVA <- manova(cbind(area_mean, smoothness_mean) ~ diagnosis, data = bC)
summary(MANOVA)
#The p value is less than 0.05. It is significant. There is a difference between area and smoothness

MANOVA1 <- manova(cbind(compactness_mean, symmetry_mean) ~ diagnosis, data = bC)
summary(MANOVA1)
#The p value is less than 0.05. It is significant. There is a difference between compactness and symmetry

MANOVA2 <- manova(cbind(concavity_mean, concave.points_mean) ~ diagnosis, data = bC)
summary(MANOVA2)
#The p value is less than 0.05. It is significant. There is a difference between compactness and symmetry