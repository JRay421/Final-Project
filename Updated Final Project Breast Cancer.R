#Load Packages
library(dplyr)

#Import Data
breast.cancer <- read.csv("C:/Users/Student/Desktop/Group Project/archive/breast-cancer.csv")
View(breast.cancer)

#Complete Data Wrangling
#Recode diagnosis
breast.cancer$diagnosisR <- NA
breast.cancer$diagnosisR[breast.cancer$diagnosis=='B'] <- 0
breast.cancer$diagnosisR[breast.cancer$diagnosis=='M'] <- 1

#Create a new dataset with our variables
breastCancer <- select(breast.cancer, c(diagnosisR,area_mean, smoothness_mean, compactness_mean, symmetry_mean))

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

#Run Logistic Regression on compactness_mean

#Test Assumptions 
#Run Base Logistic Model
mylogit1 <- glm(diagnosisR ~ compactness_mean, data=breastCancer, family='binomial')

#Predict Diagnosis
probabilities1 <- predict(mylogit1, type='response')
breastCancer$Predictedc <- ifelse(probabilities1 > .5, 'pos', 'neg')

#Recode predicted area and convert to factors
breastCancer$PredictedcR <- NA
breastCancer$PredictedcR[breastCancer$Predictedc=='pos'] <- 1
breastCancer$PredictedcR[breastCancer$Predictedc=='neg'] <- 0
breastCancer$PredictedcR <- as.factor(breastCancer$PredictedcR)
#Diagnosis was already recoded to a factor above

#Confusion Matrix
conf_mat1 <- caret::confusionMatrix(breastCancer$PredictedcR, breastCancer$diagnosisR)
conf_mat1
#None of the values are less than 5 so the sample size is met and the assumption is passed
#There is an accuracy rate of 0.7996 or 80%

#Logit Linearity
breastCancer2 <- breastCancer %>%
  dplyr::select_if(is.numeric)

predictors1 <- colnames(breastCancer2)

breastCancer2 <- breastCancer2 %>%
  mutate(logit=log(probabilities1/(1-probabilities1))) %>%
  gather(key= "predictors", value="predictor.value", -logit)

ggplot(breastCancer2, aes(logit, predictor.value))+
  geom_point(size=.5, alpha=.5)+
  geom_smooth(method= "loess")+
  theme_bw()+
  facet_wrap(~predictors, scales="free_y")
# Has a strong linear relationship

#Graphing Errors
plot(mylogit1$residuals)
#Shows a lot of outliers on the graph but appears to have even distribution

#Durbin Watson Test
dwtest(mylogit1, alternative="two.sided")
# P-value is less than 0.05 and therefore is significant. However DW is not under 1 or greater than 3 so the assumption is met

#Screen for outliers
infl1 <- influence.measures(mylogit1)
summary(infl1)

#Logistic Regression
summary(mylogit1)
#The p-value is less than 0.05 and is significant. Compactness_mean is a significant predictor of diagnosis. 
#There is an increase of 36.38 with compactness in regards to diagnosis.

#Graphing Logistic Model
logi.hist.plot(breastCancer$compactness_mean,breastCancer$diagnosisR, boxp=FALSE, type="hist", col="gray")
