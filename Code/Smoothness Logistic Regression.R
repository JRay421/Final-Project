#Completed by Sarah Hagans
#Smoothness Binary Logistic Model

#Logistic regression Libraries
library("caret")
library("IDPmisc")
library("magrittr")
library("dplyr")
library("tidyr")
library("lmtest")
library("popbio")
library("e1071")

#Base Logistic Model
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
#All cells were above 5 which is good the assumption has been met.
#Accuracy rating is 0.6766.

#Logit Linearity
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
#p- value is significant being less than 0.05
#Wald stat is 7.959
#smoothness_mean estimate influence is 60.0857
#AIC is 677.95

#graphing it for a figure
logi.hist.plot(BC$smoothness_mean,BC$diagnosisR, boxp=FALSE, type="hist", col="gray")
#very flat don't use
