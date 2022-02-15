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