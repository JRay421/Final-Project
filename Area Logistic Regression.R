#Completed by Jasmine Ray
#Run Logistic Regression on area_mean

#Load Libraries
library("caret")
library("magrittr")
library("dplyr")
library("tidyr")
library("lmtest")
library("popbio")
library("e1071")

#Test Assumptions 
#Run Base Logistic Model
mylogit <- glm(diagnosisR ~ area_mean, data=breastCancer, family='binomial')

#Predict Diagnosis
probabilities <- predict(mylogit, type='response')
breastCancer$Predicteda <- ifelse(probabilities > .5, 'pos', 'neg')

#Recode predicted area and convert to factors
breastCancer$PredictedaR <- NA
breastCancer$PredictedaR[breastCancer$Predicteda=='pos'] <- 1
breastCancer$PredictedaR[breastCancer$Predicteda=='neg'] <- 0
breastCancer$PredictedaR <- as.factor(breastCancer$PredictedaR)
breastCancer$diagnosisR <- as.factor(breastCancer$diagnosisR)

#Confusion Matrix
conf_mat <- caret::confusionMatrix(breastCancer$PredictedaR, breastCancer$diagnosisR)
conf_mat
#None of the values are less than 5 so the sample size is met and the assumption is passed
#There is an accuracy rate of 0.884 or 88%

#Logit Linearity
breastCancer1 <- breastCancer %>%
dplyr::select_if(is.numeric)

predictors <- colnames(breastCancer1)

breastCancer1 <- breastCancer1 %>%
  mutate(logit=log(probabilities/(1-probabilities))) %>%
  gather(key= "predictors", value="predictor.value", -logit)

ggplot(breastCancer1, aes(logit, predictor.value))+
  geom_point(size=.5, alpha=.5)+
  geom_smooth(method= "loess")+
  theme_bw()+
  facet_wrap(~predictors, scales="free_y")
# Has a strong linear relationship

#Graphing Errors
plot(mylogit$residuals)
#Shows a lot of outliers on the graph but appears to have even distribution

#Durbin Watson Test
dwtest(mylogit, alternative="two.sided")
# P-value is less than 0.05 and therefore is significant. However DW is not under 1 or greater than 3 so the assumption is met

#Screen for outliers
infl <- influence.measures(mylogit)
summary(infl)

#Logistic Regression
summary(mylogit)
#The p-value is less than 0.05 and is significant. Area_mean is a significant predictor of diagnosis. 
#There is an increase of 0.01 with area

#Graphing Logistic Model
logi.hist.plot(breastCancer$area_mean,breastCancer$diagnosisR, boxp=FALSE, type="hist", col="gray")