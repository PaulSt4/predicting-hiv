rm(list = ls())

library(tidyverse)

load("../Data/project data.RData")
rm(dropped)

n = 1:nrow(D0)
se = rep(NA, nrow(D0))
ae = rep(NA, nrow(D0))
prediction = rep(NA, nrow(D0))
for (i in n) {
  # i = 1
  model1 = lm(HIVper100000 ~ ., data = D0[-i,])
  prediction[i] = predict(model1, newdata = D0[i,])
  
  se[i] = (D0[i,1]-prediction[i])^2
  ae[i] = abs(D0[i,1]-prediction[i])
}

R2 = 1-(sum(se)/sum((D0[,1]-mean(D0[,1]))^2))

MSE = mean(se)
MAE = mean(ae)


