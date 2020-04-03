rm(list = ls())

library(tidyverse)
library(glmnet)
library(rsample) 

load("../Data/project data.RData")
rm(dropped)

# split into training and testing data

set.seed(123) 
hiv_split = initial_split(D0_orig, prop = .7)
hiv_train = training(hiv_split)
hiv_test  = testing(hiv_split)
rm(hiv_split)

# create model matrices

hiv_train_x <- model.matrix(HIVper100000 ~ ., hiv_train)[, -1]
hiv_train_y <- log(hiv_train$HIVper100000)

hiv_test_x <- model.matrix(HIVper100000 ~ ., hiv_test)[, -1]
hiv_test_y <- log(hiv_test$HIVper100000)

# lasso 
hiv_lasso <- glmnet(
  x = hiv_train_x,
  y = hiv_train_y,
  alpha = 1,
  standardize = TRUE
)

plot(hiv_lasso, xvar = "lambda")
hiv_lasso$lambda

# cv lasso
hiv_lassoCV <- cv.glmnet(
  x = hiv_train_x,
  y = hiv_train_y,
  alpha = 1,
  standardize = TRUE
)

plot(hiv_lassoCV)

min(hiv_lassoCV$cvm)
hiv_lassoCV$lambda.min 

coef(hiv_lassoCV, s = "lambda.1se")


# testing
pred <- predict(hiv_lassoCV, hiv_test_x, s = "lambda.1se")

pred_normal = exp(pred)

MSE = mean((hiv_test[,1] - pred_normal)^2)
RMSE = sqrt(MSE)
MAE = mean(abs(hiv_test[,1] - pred_normal))
