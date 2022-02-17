#Completed by Candy Linde
#Concave Points Logistic Regression

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

#Base Logistic Model
logit <- glm(diagnosisR ~ concave.points_mean, data=BC, family="binomial")

#Predict diagnosis and recode predicted variables
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

#All cells are above 5 and the accuracy rate is 90%. The assumption has been met.


#Logit Linearity - gather only numeric variables
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
#what I notice in this that the concave points mean is right along the line and concavity mean is also in a pretty straight line.

#Graphing the Errors
plot(logit$residuals)
#pretty even distribution.

#check for independence of errors
dwtest(logit, alternative="two.sided")
#since the DW value is 1.7677 this meets the assumption of independence errors

#screen for outliers
infl <- influence.measures(logit)
summary(infl)
#these numbers look good with no or low outliers

#Running Logistic Regression
summary(logit)
#The p-value is less than 0.05 and shows significance. 
#Wald stat is 9.11
#concave.points_mean estimate influence is 106.99
