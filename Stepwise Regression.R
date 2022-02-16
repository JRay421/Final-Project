# Completed by Candy Linde

#stepwise linear regression
BCFit1 = lm(diagnosis ~ ., data = BC)
summary(BCFit1)

#Backward Elimination
step(BCFit1, direction = 'backward')
#The best model provided is the following: 2.187 - 0.0003(area_mean) - 7.467(concave.points_mean)

BCFitSome1 = lm(diagnosis ~ area_mean + concave.points_mean, data = BC)
summary(BCFitSome1)
#The results tells us that the adjusted R-square value is 0.6168.
#The multiple R-square value is 0.6182. This shows that the model explains 61.82% of the variation in diagnosis.
#The p-value is less than 0.05 showing this model is significant. 


#Forward Elimination
BCFit = lm(diagnosis ~ 1, data = BC)
summary(BCFit)
step(BCFit, direction = 'forward', scope = (~area_mean + smoothness_mean + compactness_mean + symmetry_mean + concave.points_mean + concavity_mean))
#The best model provided is the following: 2.187 - 7.467(concave.points_mean) - 0.0003(area_mean) 

BCFitSome = lm(diagnosis ~ concave.points_mean + area_mean, data = BC)
summary(BCFitSome)
#The adjusted R-squared value is 0.6168 and the p-value is less than 0.05
#Both forward and backward stepwise regression shows us that the model will be 62% accurate and is significant since the p-value is below 0.05.  
#The model to be used only keeps two of the 6 variables.
