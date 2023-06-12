# Notes & Lecture by Akhona Njeje.
# Date 10 June 2023.
# Topic : Machine Learning & Algorithms.Decision Trees & Random Forests.
# Doc & Extra read ---> Intro to Stats Learning Chapt 8 by Gareth James.


# Theory:

# Where can you apply these Algorithms?
# Given a dataset on weather, y = should the NFL game take place , options are YES/NO. 
# This is a task we can give to Decision Trees.

# Get the data. Predict based on Parameters that a certain varsity is public/private school = y = Yes/No.

library(ISLR)
head(College)

df = College

library(ggplot2) # Scatter plot.
ggplot(df, aes(Room.Board, Grad.Rate)) + geom_point(aes(color = Private))

ggplot(df, aes(F.Undergrad)) + geom_histogram(aes(fill=Private), color='black', 
                                              bins = 50, alpha = 0.5)
# Grad rate hist.
ggplot(df, aes(Grad.Rate)) + geom_histogram(aes(fill=Private), color='black',
                                            bins = 50, alpha = 0.5)
# Wich School has a Grad Rate of over 100%?
subset(df, Grad.Rate > 100) # = Cazenovia College.

# Change 118% to 100% pass rate.
df['Cazenovia College', 'Grad.Rate'] = 100


# Train/Test Split.

library(caTools)
set.seed(101)

sample = sample.split(df$Private, SplitRatio = .70)
train = subset(df, sample==TRUE)
test = subset(df, sample==FALSE)


library(rpart)

tree = rpart(Private ~ ., method = 'class', data = train)
summary(tree)

tree.preds = predict(tree, test)
head(tree.preds) # = Get probabilities. The model is 99.66887% sure that 
                 #   Adrian College is a private school.

tree.preds = as.data.frame(tree.preds)

joiner = function(x){
  if (x >= 0.5){
    return('Yes')
  }else{
    return('No')
  }
}
tree.preds$Private = sapply(tree.preds$Yes, joiner)
print(head(tree.preds))

# Confusion Matrix.

table(tree.preds$Private, test$Private)

install.packages("rpart.plot")
library(rpart.plot)
prp(tree)

# Random Forest Model.

install.packages("randomForest")
library(randomForest)

rf.model = randomForest(Private ~ ., data=train,
                        importance=TRUE)

rf.model$confusion

rf.model$importance

rf.preds = predict(rf.model, test)
table(rf.preds, test$Private)