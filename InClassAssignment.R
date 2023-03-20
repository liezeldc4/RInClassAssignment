#Multiple Linear Regression


library(tidyverse)

dataset=read.csv('heart.data.csv')

dataset2=read.csv('Heart_validation.csv')



#Exploring the Heart Data dataset

view(dataset)

glimpse(dataset)

length(dataset)

names(dataset)

summary(dataset)



#Exploring the Heart Validation dataset

view(dataset2)

glimpse(dataset2)

length(dataset2)

names(dataset2)

summary(dataset2)



#Checking both datasets for missing values

colSums(is.na(dataset))

colSums(is.na(dataset2))



#Plotting out the Heart Data dataset for Visualization

#Histogram is unimodal, moderately distributed, slightly right-skewed

ggplot(data=dataset,
       
       aes(heart.disease))+
  
  geom_histogram()

# Impute the missing data in dataset$biking
biking_median = median(dataset$biking, na.rm = TRUE)
dataset$biking = ifelse(is.na(dataset$biking), biking_median, dataset$biking)
colSums(is.na(dataset))

# Impute the missing data in dataset$smoking
smoking_median = median(dataset$smoking, na.rm = TRUE)
dataset$smoking = ifelse(is.na(dataset$smoking), smoking_median, dataset$smoking)
colSums(is.na(dataset))

# Impute the missing data in dataset$heart.disease
heart.disease_median = median(dataset$heart.disease, na.rm = TRUE)
dataset$heart.disease= ifelse(is.na(dataset$heart.disease), heart.disease_median, dataset$heart.disease)
colSums(is.na(dataset))

#Splitting the data into two sets

library(caTools)

set.seed(100)

split=sample.split(dataset$heart.disease, SplitRatio = 0.8) #80% Training, 20% Testing

training_set=subset(dataset,split==TRUE)

testing_set=subset(dataset,split==FALSE)



# Multiple Linear Regression Training

names(dataset)

MLR=lm(formula=heart.disease~.,
       
       data=training_set)

summary(MLR)
# heart disease = 14.988608 -0.200321(biking) +0.178403(smoking)



# Mean Square Error

summary=summary(MLR)

MSE=(mean(summary$residuals^2))

paste("The Mean Squared Error is: ", MSE)



# R Square = 0.9781
summary(MLR)

# Testing Set Prediction for heart disease
y_pred = predict(MLR, newdata = testing_set)
data=data.frame(testing_set$heart.disease, y_pred)
head(data)

# Compare to actual values. Read Validation file.
new=read.csv('Heart_validation.csv')
head(new)

# Remove last column. We want to predict that column.
new_x=new[c(1:2)]
new_x
data.frame(new[c(3)],predict(MLR, newdata=new_x))

# Here we can see the predicted values are very close indeed to the actual values.
# Which makes sense since the MSE was close to 0.
# Thus, this dataset is useful and closely accurate at predicting heart disease.