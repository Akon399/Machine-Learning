# Notes & Lecture by Akhona Njeje,Date 25 May 2023.
# Advance Data Analysis & Visualisation with GGPLOT2.
# Problem St : Student Perfomamance.
# Use ML Algorithm to predict final grades for students.

df = read.csv("D:/Users/NjejeA/Downloads/My Research Projects/Raw Solutions/R Solutions/Machine Learning/Part 3 Linear Regression/student.csv", sep = ';')
head(df)

# Descriptive Statistics.

summary(df)

# Check Null Values.
any(is.na(df))   # FALSE = clean dataset.

# Check Columns Data types.
str(df)


# EDA.

library(ggplot2)
library(ggthemes)
library(dplyr)


# Correlation Plots.
   # Call only columns with numeric values for Corr plots.

num.cols = sapply(df,is.numeric)
num.cols

corr.data = cor(df[,num.cols])   # This is our Correlation data.
head(corr.data)

install.packages("corrgram")
install.packages("corrplot")

library(corrgram)
library(corrplot)

# Lets begin plotting a heatmap from corrplot library.

print(corrplot(corr.data,method = 'color'))   # = Heatmap.

# 

corrgram(df)   # Heatmap.

# Lets begin plotting a histogram.

ggplot(df,aes(x=G3)) + geom_histogram(bins=50,alpha=0.5,fill='blue')   # Looks like alot of people are failing the tests.



# Model Development : Linear Regression.



# Data Spliling between Training & Testing.

install.packages("caTools")   # This library helps us split our data easily.
library(caTools)

# Set a seed. Random selection in our data.
set.seed(101)

# Split up sample.
sample = sample.split(df$G3, SplitRatio = 0.8)   # Our training data is 80% & we predicting our outcome using Column G3.

train = subset(df,sample==TRUE) # 80% of data goes to training.
test = subset(df, sample==FALSE) # 20% of data goes to testing.

# Lets train $ build our model.

model = lm(G3 ~ ., data=train)   # G3 = y.
print(summary(model))   # Information on our model.

# Now that you have rich info in your model plot the residuals.
residualz = residuals(model)
residualz = as.data.frame(residualz)
head(residualz) 

ggplot(residualz,aes(residualz)) + geom_histogram(fill='red', alpha=0.5)
# The residual histogram is predicting that some students will get negatives, & the lowest has to zero not -1/-5.Lets fix it. 
# We want our residulas to be normal.


# Advance Visulisation of our model.



plot(model)   # You'll get 4 Visuals for our model.

# Lets begin Prediction by using Test dataset.
G3.predictions = predict(model,test)
results = cbind(G3.predictions,test$G3)
colnames(results) = c('predicted','actual')

print(head(results))

# Lets take of zeros in our HIstogram.

to_zero = function(x){
  if(x<0){
    return(0)
  }else{
    return(x)
  }
}

# lets apply the zero function.

results$predicted = sapply(results$predicted,to_zero)

# MSE.
mse = mean((results$actual - results$predicted)^2)
print('MSE')
print(mse)







