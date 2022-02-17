#Histograms for all variables

#Load Library    concavity and concave points
library(rcompanion)

plotNormalHistogram(BC$area_mean)
#Positive Skew
area_meanSQRT <- sqrt(BC$area_mean)
plotNormalHistogram(area_meanSQRT)
#Still Slightly  positive skew
area_meanLOG <- log(BC$area_mean)
plotNormalHistogram(area_meanLOG)
#Normal Distribution

plotNormalHistogram(BC$smoothness_mean)
#Normal Distribution

plotNormalHistogram(BC$compactness_mean)
#Positive Skew
compactness_meanSQRT <- sqrt(BC$compactness_mean)
plotNormalHistogram(compactness_meanSQRT)
#Still has a slight positive skew
compactness_meanLOG <- log(BC$compactness_mean)
plotNormalHistogram(compactness_meanLOG)
#More of a normal distribution

plotNormalHistogram(BC$symmetry_mean)
#Normal Distribution

plotNormalHistogram(BC$concave.points_mean)
#Slight positive skew
concave.points_meanSQRT <- sqrt(BC$concave.points_mean)
plotNormalHistogram(concave.points_meanSQRT)
#More of a normal distribution

plotNormalHistogram(BC$concavity_mean)
#Slight positive skew
concavity_meanSQRT <- sqrt(BC$concavity_mean)
plotNormalHistogram(concavity_meanSQRT)
#Normal Distribution