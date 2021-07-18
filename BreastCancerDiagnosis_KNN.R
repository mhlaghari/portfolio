#Diagnosing breast cancer with KNN Algorithm 
#We will run the knn algorithm to solve for a dependent variable 'Diagnosis' 
#to see whether we can train our model to detect Malignant or Benign

# Loading the dataset and libraries
library(dplyr)
wbcd <- read.csv('wisc_bc_data.csv', stringsAsFactors = FALSE)

#Removing the ID column (as it is unncessary for the analysis)
wbcd <- wbcd[-1]
table(wbcd$diagnosis)

#Changing the B and M to Benign and Malignant
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c('B','M'), labels = c('Benign','Malignant'))
round(prop.table(table(wbcd$diagnosis))*100, digits = 1)

#Viewing the summary statistics 
summary(wbcd[c('radius_mean','area_mean','smoothness_mean')])

#As the statistics were not scaled, I created a function called normalize that would 
#iterate through the data and scale everything between 0 & 1 
normalize <- function(x) {
  return ((x - min(x))/
            (max(x) - min(x)))
}
normalize(c(1,2,3,4,5))

wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
summary(wbcd_n$area_mean)

#Determining the Train/ Test split. 
#Testing the model on 100 obersvations
wbcd_train <- wbcd_n[1:469,]
wbcd_test <- wbcd_n[470:569,]

wbcd_train_labels <- wbcd[1:469,1]
wbcd_test_labels <- wbcd[470:569,1]

install.packages('class')
library(class)

#Set K=21 No real logic behind it, can be anything, but should be an odd number, to have no ties.
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=21)
install.packages('gmodels')
library(gmodels)

CrossTable(x = wbcd_test_labels, y= wbcd_test_pred, prop.chisq = FALSE)

# The model was 98% accurate. 77 True-Negatives, 21 True-Positives and 2 False-Positives

#fine-tuning the model with Transformation Z-score standardization
wbcd_z <- as.data.frame(scale(wbcd[-1]))
summary(wbcd_z$area_mean)


wbcd_train <- wbcd_z[1:469,]
wbcd_test <- wbcd_z[470:569,]
wbcd_train_labels <- wbcd[1:469,1]
wbcd_test_labels <- wbcd[470:569,1]
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=21)
CrossTable(x = wbcd_test_labels, y= wbcd_test_pred, prop.chisq = FALSE)

#Same results as the previous model. 77 True-Negatives, 21 True-Positives and 2 False-Positives

