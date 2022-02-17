#Completed by Candy Linde
#Binary logistic regression for concavity mean 

#Base Logistic Model
logit <- glm(diagnosisR ~ concavity_mean, data=BC, family="binomial")

#predict diagnosis and recode variable
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

#Logit Linearity - gather only numeric variables
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

#Graphing the Errors
plot(logit$residuals)
#distribution looks pretty even

#check for independence of errors
dwtest(logit, alternative="two.sided")
#since the DW value is 1.6898 this meets the assumption of independence errors

#screen for outliers
infl <- influence.measures(logit)
summary(infl)
#these numbers look good with no or low outliers

#Running Logistic Regression
summary(logit)
#The p-value is less than 0.05 and shows significance. 
#Wald stat is 12.15
#concave.points_mean estimate influence is 36.85