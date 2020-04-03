rm(list = ls())

library(rsample)      
library(randomForest) 
library(ranger)       
library(caret)      
library(tidyverse)

load("../Data/project data.RData")
rm(dropped)

# split data
set.seed(123) 
hiv_split = initial_split(D0, prop = .7)
hiv_train = training(hiv_split)
hiv_test  = testing(hiv_split)
rm(hiv_split)


# default random forest
set.seed(123)
model1 <- randomForest(
  formula = HIVper100000 ~ .,
  data    = hiv_train
)

model1

plot(model1)

which.min(model1$mse)
sqrt(model1$mse[which.min(model1$mse)])


# caret
library(caret)
rf = train(HIVper100000~., data = D0, method = "rf", trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3, search = "random"), tuneLength = 15)
plot(rf)

predict(rf, newdata = D_NA)


