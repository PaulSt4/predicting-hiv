rm(list = ls())

library(rsample)     
library(dplyr)       
library(rpart)      
library(rpart.plot) 
library(ipred)       
library(caret)       
library(e1071)

load("../Data/project data.RData")
rm(dropped)

model1 = rpart(HIVper100000 ~ ., data = D0, cp = 0)

plot(model1, uniform=TRUE, main="Regression tree for HIV")
text(model1, use.n=TRUE, all=TRUE, cex=.8, pretty = 0)




