



#load in library
library("ggplot2")

##
#Create new df with the selected variables
BC <- dplyr:: select(breast.cancer, c(diagnosis,area_mean, smoothness_mean, compactness_mean, symmetry_mean, concavity_mean, concave.points_mean))
BC

#Recode the categorical data to numerical data
BC$diagnosis[BC$diagnosis == "M"] <- 0
BC$diagnosis[BC$diagnosis == "B"] <- 1
BC


#Modeling using stepwise regression
fitall = lm(diagnosis ~ ., data = BC)
formula(fitall)
summary(fitall)

fitstart = lm(diagnosis ~ 1, data = BC)
head(BC)

step(fitstart, direction = "both", scope = formula(fitall))
##

#Show Dataset
head(BC)
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
#Change name of column with spaces
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
#End Sarah's work#

##Begin Sarah's Symmetry logit##
#Sarah's Symmetry Basic Logistic Model
symlogit <- glm(diagnosisR ~ symmetry_mean, data=BC, family="binomial")

#predict Diagnosis
probabilitiesym <- predict(symlogit, type = "response")
#Recode Predicted variable
BC$Predictedsym <- ifelse(probabilitiesym > .5, "pos", "neg")
BC$PredictedsymR <- NA
BC$PredictedsymR[BC$Predictedsym =='pos'] <- 1
BC$PredictedsymR[BC$Predictedsym =='neg'] <- 0
#Convert Variables to Factors
BC$PredictedsymR <- as.factor(BC$PredictedsymR)
BC$diagnosisR <- as.factor(BC$diagnosisR)
#Create Confusion Matrix
conf_matsym <- caret::confusionMatrix(BC$PredictedsymR, BC$diagnosisR)
conf_matsym
#All cells were above 5 which is good.
#Accuracy rating is 0.6819.
#Logit Linearity
#gather only numeric variables
symmetry1 <- BC %>%
  dplyr::select_if(is.numeric)

#pull them to be fed into predictors
predictors_symmetry <- colnames(symmetry1)

#create final smooth logit
symmetry1 <- symmetry1 %>%
  mutate(logit=log(probabilitiesym/(1-probabilitiesym))) %>%
  gather(key= "predictors", value = "predictor.value", -logit)

#Graphing to assess linearity
ggplot(symmetry1, aes(logit, predictor.value))+
  geom_point(size=.5, alpha=.5)+
  geom_smooth(method= "loess")+
  theme_bw()+
  facet_wrap(~predictors, scales="free_y")
#wow again strong line symmetry_mean it is a negative slope.
#graph residuals
plot(symlogit$residuals)
#less even distribution than smoothness but I think it is even enough.

#Screen for outliers
inflsym <- influence.measures(symlogit)
summary(inflsym)
#dfb.1_ and dffitvalues are below 1 and hat values are below 0.3.

#running logistic regression and interpreting out put
summary(symlogit)
#p- value is sig at 0.001
#Wald stat is -7.977
#smoothness_mean estimate influence is -27.6042
#AIC is 690.8

#graphing it for a figure
logi.hist.plot(BC$symmetry_mean,BC$diagnosisR, boxp=FALSE, type="hist", col="gray")
#very flat don't use

