rm(list = ls())

library(tidyverse)
library(glmnet)
library(rsample) 

load("../Data/project data.RData")
rm(dropped)

# # bad try 1 ----
# 
# # split into training and testing data
# 
# set.seed(123) 
# hiv_split = initial_split(D0, prop = .7)
# hiv_train = training(hiv_split)
# hiv_test  = testing(hiv_split)
# rm(hiv_split)
# 
# # create model matrices
# 
# hiv_train_x <- model.matrix(HIVper100000 ~ ., hiv_train)[, -1]
# hiv_train_y <- scale(hiv_train$HIVper100000)
# 
# hiv_test_x <- model.matrix(HIVper100000 ~ ., hiv_test)[, -1]
# hiv_test_y <- scale(hiv_test$HIVper100000)
# 
# # lasso 
# hiv_lasso <- glmnet(
#   x = hiv_train_x,
#   y = hiv_train_y,
#   alpha = 1,
#   standardize = FALSE
# )
# 
# plot(hiv_lasso, xvar = "lambda")
# hiv_lasso$lambda
# 
# # cv lasso
# hiv_lassoCV <- cv.glmnet(
#   x = hiv_train_x,
#   y = hiv_train_y,
#   alpha = 1,
#   standardize = FALSE
# )
# 
# plot(hiv_lassoCV)
# 
# min(hiv_lassoCV$cvm)
# hiv_lassoCV$lambda.min 
# 
# coef(hiv_lassoCV, s = "lambda.1se")
# 
# 
# # testing
# pred <- predict(hiv_lassoCV, hiv_test_x, s = "lambda.1se")
# 
# pred_normal = pred * attr(hiv_test_y, 'scaled:scale') + attr(hiv_test_y, 'scaled:center')
# 
# MSE = mean((hiv_test[,1] - pred_normal)^2)
# RMSE = sqrt(MSE)
# MAE = mean(abs(hiv_test[,1] - pred_normal))

# try 2 ----
# create model matrices

hiv_train_x <- model.matrix(HIVper100000 ~ ., D0)[, -1]
hiv_train_y <- scale(D0$HIVper100000)

hiv_test_x <- model.matrix(HIVper100000 ~ ., D0)[, -1]
hiv_test_y <- scale(D0$HIVper100000)


# cv lasso
hiv_lassoCV <- cv.glmnet(
  x = hiv_train_x,
  y = hiv_train_y,
  alpha = 1,
  nfolds = 10,
  standardize = FALSE
)

plot(hiv_lassoCV)

min(hiv_lassoCV$cvm)
lambda_lasso = hiv_lassoCV$lambda.1se
coef(hiv_lassoCV, s = "lambda.1se")


# training error
pred <- predict(hiv_lassoCV, hiv_test_x, s = "lambda.1se")

pred_normal = pred * attr(hiv_test_y, 'scaled:scale') + attr(hiv_test_y, 'scaled:center')

MSE = mean((D0[,1] - pred_normal)^2)
RMSE = sqrt(MSE)
MAE = mean(abs(D0[,1] - pred_normal))

# train model 
hiv_lasso <- glmnet(
  x = hiv_train_x,
  y = hiv_train_y,
  alpha = 1,
  lambda = lambda_lasso,
  standardize = FALSE
)


# ridge -----
# cv ridge
hiv_ridgeCV <- cv.glmnet(
  x = hiv_train_x,
  y = hiv_train_y,
  alpha = 0,
  nfolds = 10,
  standardize = FALSE
)

plot(hiv_ridgeCV)

min(hiv_ridgeCV$cvm)
lambda_ridge = hiv_ridgeCV$lambda.1se 
coef(hiv_ridgeCV, s = "lambda.1se")


# training error
pred_r <- predict(hiv_ridgeCV, hiv_test_x, s = "lambda.1se")

pred_r_normal = pred_r * attr(hiv_test_y, 'scaled:scale') + attr(hiv_test_y, 'scaled:center')

MSE = mean((D0[,1] - pred_r_normal)^2)
RMSE = sqrt(MSE)
MAE = mean(abs(D0[,1] - pred_r_normal))

# train model 
hiv_ridge <- glmnet(
  x = hiv_train_x,
  y = hiv_train_y,
  alpha = 0,
  lambda = lambda_ridge,
  standardize = FALSE
)

1 - hiv_ridgeCV$cvm/var(hiv_train_y)
