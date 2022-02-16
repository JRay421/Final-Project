#Completed by Sarah Hagan
#Symmetry Binary Logistic Model
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
#All cells were above 5 which is good and the assumption is met.
#Accuracy rating is 0.6819.

#Logit Linearity - gather only numeric variables
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
#p- value is significant at less than 0.05
#Wald stat is -7.977
#smoothness_mean estimate influence is 27.6042
#AIC is 690.8

#graphing it for a figure
logi.hist.plot(BC$symmetry_mean,BC$diagnosisR, boxp=FALSE, type="hist", col="gray")
#very flat don't use
