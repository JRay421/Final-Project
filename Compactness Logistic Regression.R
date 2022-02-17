#Completed by Jasmine Ray
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

