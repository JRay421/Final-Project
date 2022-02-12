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
library("IDPmisc")
library("magrittr")
library("dplyr")
library("tidyr")
library("lmtest")
library("popbio")
library("e1071")

#Begin Sarah's work#
#I have the concave points variable as **concave.points_mean**

#Sarah's Smoothness Basic Logistic Model
summary(BC)
BC$diagnosisR <- NA
BC$diagnosisR[BC$diagnosis=='1'] <- 1
BC$diagnosisR[BC$diagnosis=='0'] <- 0
summary(BC)
#Remove missing Variables
BC$diagnosisR <- as.numeric(BC$diagnosisR)
summary(BC)

smoothlogit <- glm(diagnosisR ~ smoothness_mean, data=BC, family="binomial")
#predict Diagnosis
probabilitiesmooth <- predict(smoothlogit, type = "response")
#Recode Predicted variable
BC$Predicted <- ifelse(probabilitiesmooth > .5, "pos", "neg")
BC$PredictedR <- NA
BC$PredictedR[BC$Predicted =='pos'] <- 1
BC$PredictedR[BC$Predicted =='neg'] <- 0

#Convert Variables to Factors
BC$PredictedR <- as.factor(BC$PredictedR)
BC$diagnosisR <- as.factor(BC$diagnosisR)

#Create Confusion Matrix
conf_mat <- caret::confusionMatrix(BC$PredictedR, BC$diagnosisR)
conf_mat
#All cells were above 5 which is good.
#Accuracy rating is 0.6766.

#Logit Linearity
#gather only numeric variables
smooth1 <- BC %>%
dplyr::select_if(is.numeric)

#pull them to be fed into predictors
predictors_smooth <- colnames(smooth1)
#create final smooth logit
smooth1 <- smooth1 %>%
mutate(logit=log(probabilitiesmooth/(1-probabilitiesmooth))) %>%
gather(key= "predictors", value = "predictor.value", -logit)
#Graphing to assess linearity
ggplot(smooth1, aes(logit, predictor.value))+
  geom_point(size=.5, alpha=.5)+
  geom_smooth(method= "loess")+
  theme_bw()+
  facet_wrap(~predictors, scales="free_y")
#wow what a strong line smoothness_mean it is a negative slope.
#graph residuals
plot(smoothlogit$residuals)
#pretty even distribution.

#Screen for outliers
infl <- influence.measures(smoothlogit)
summary(infl)
#dfb.1_ and dffitvalues are below 1 and hat values are below 0.3.
#running logistic regression and interpreting out put
summary(smoothlogit)
#p- value is sig at0.001
#Wald stat is -7.959
#smoothness_mean estimate influence is -60.0857
#AIC is 677.95

#graphing it for a figure
logi.hist.plot(BC$smoothness_mean,BC$diagnosisR, boxp=FALSE, type="hist", col="gray")
#very flat don't use
##
#End Sarah's work#
###

#Candy's Binary Logistic Regression with confusion matrixs
#Load libraries

library("dplyr")
library("ggplot2")
library("caret")
library("IDPmisc")
library("magrittr")
library("dplyr")
library("tidyr")
library("lmtest")
library("popbio")
library("e1071")
library("zoo")

#Select variables that you want to keep
BC <- dplyr:: select(breast_cancer, c(diagnosis,area_mean, smoothness_mean, compactness_mean, symmetry_mean, concave_points_mean, concavity_mean))

#Change to binary
BC$diagnosisR <- NA
BC$diagnosisR[BC$diagnosis=='1'] <- 1
BC$diagnosisR[BC$diagnosis=='0'] <- 0
summary(BC)

#Remove missing Variables
BC$diagnosisR <- as.numeric(BC$diagnosisR)
summary(BC)

#Predict diagnosis and recode predicted variables
logit <- glm(diagnosisR ~ concave_points_mean, data=BC, family="binomial")
probabilities <- predict(logit, type = "response")
BC$Predicted <- ifelse(probabilities > .5, "pos", "neg")
BC$PredictedR <- NA
BC$PredictedR[BC$Predicted =='pos'] <- 1
BC$PredictedR[BC$Predicted =='neg'] <- 0

#Convert variables to factors
BC$PredictedR <- as.factor(BC$PredictedR)
BC$diagnosisR <- as.factor(BC$diagnosisR)

#create confusion matrix
conf_mat <- caret::confusionMatrix(BC$PredictedR, BC$diagnosisR)
conf_mat
#Confusion Matrix and Statistics

#Reference
#Prediction   0   1
#0 334  30
#1  23 182

#Accuracy : 0.9069          
#95% CI : (0.8799, 0.9294)
#No Information Rate : 0.6274          
#P-Value [Acc > NIR] : <2e-16          

#Kappa : 0.7994          

#Mcnemar's Test P-Value : 0.4098          
                                          
           # Sensitivity : 0.9356          
           # Specificity : 0.8585          
         #Pos Pred Value : 0.9176          
         #Neg Pred Value : 0.8878          
            # Prevalence : 0.6274          
        # Detection Rate : 0.5870          
  # Detection Prevalence : 0.6397          
    #  Balanced Accuracy : 0.8970          
                                          
     #  'Positive' Class : 0               
                                          
#All cells are above 5 and the accuracy rate is 90%


#Logit Linearity
#gather only numeric variables
concave_points_mean <- BC %>%
dplyr::select_if(is.numeric)

#pull them to be fed into predictors
predictors <- colnames(concave_points_mean)

#create final logit
concave_points_mean <- concave_points_mean %>%
mutate(logit=log(probabilities/(1-probabilities)))%>% 
gather(key= "predictors", value = "predictor.value", -logit)

#Graphing to assess linearity
ggplot(concave_points_mean, aes(logit, predictor.value))+
  geom_point(size=.5, alpha=.5)+
  geom_smooth(method= "loess")+
  theme_bw()+
  facet_wrap(~predictors, scales="free_y")
#what I notice in this that the concave points mean is right along the line and 
#concavity mean is also in a pretty straight line.

plot(logit$residuals)
#pretty even distribution.

#check for independence of errors
dwtest(logit, alternative="two.sided")
#since the DW value is 1.7677 this meets the assumption of independence errors

#screen for outliers
infl <- influence.measures(logit)
summary(infl)
#these numbers look good with no or low outliers

#Binary linearity for concavity mean variable
#predict diagnosis and recode variable
logit <- glm(diagnosisR ~ concavity_mean, data=BC, family="binomial")
probabilities1 <- predict(logit, type = "response")
BC$Predicted <- ifelse(probabilities1 > .5, "pos", "neg")
BC$PredictedR <- NA
BC$PredictedR[BC$Predicted =='pos'] <- 1
BC$PredictedR[BC$Predicted =='neg'] <- 0

#Convert variables to factors
BC$PredictedR <- as.factor(BC$PredictedR)
BC$diagnosisR <- as.factor(BC$diagnosisR)


#create confusion matrix
conf_mat <- caret::confusionMatrix(BC$PredictedR, BC$diagnosisR)
conf_mat


#Logit Linearity
#gather only numeric variables
concavity_mean <- BC %>%
  dplyr::select_if(is.numeric)

#pull them to be fed into predictors
predictors <- colnames(concavity_mean)

#create final logit
concavity_mean <- concavity_mean %>%
  mutate(logit=log(probabilities1/(1-probabilities1)))%>% 
  gather(key= "predictors", value = "predictor.value", -logit)

#Graphing to assess linearity
ggplot(concavity_mean, aes(logit, predictor.value))+
  geom_point(size=.5, alpha=.5)+
  geom_smooth(method= "loess")+
  theme_bw()+
  facet_wrap(~predictors, scales="free_y")
#this also graphs right on the line

plot(logit$residuals)
#distribution looks pretty even

#check for independence of errors
dwtest(logit, alternative="two.sided")
#since the DW value is 1.6898 this meets the assumption of independence errors

#screen for outliers
infl <- influence.measures(logit)
summary(infl)
#these numbers look good with no or low outliers



#histograms for all the values

plotNormalHistogram(BC$area_mean)
plotNormalHistogram(BC$smoothness_mean)
plotNormalHistogram(BC$compactness_mean)
plotNormalHistogram(BC$symmetry_mean)
plotNormalHistogram(BC$concave_points_mean)
plotNormalHistogram(BC$concavity_mean)

area_meanSQRT <- sqrt(BC$area_mean)
plotNormalHistogram(area_meanSQRT)
area_meanLOG <- log(BC$area_mean)
plotNormalHistogram(area_meanLOG)

compactness_meanSQRT <- sqrt(BC$compactness_mean)
plotNormalHistogram(compactness_meanSQRT)
compactness_meanLOG <- log(BC$compactness_mean)
plotNormalHistogram(compactness_meanLOG)

concave_points_meanSQRT <- sqrt(BC$concave_points_mean)
plotNormalHistogram(concave_points_meanSQRT)


concavity_meanSQRT <- sqrt(BC$concavity_mean)
plotNormalHistogram(concavity_meanSQRT)

#smoothness and symmetry didn't need to be adjusted they had a perfect bell curve.























