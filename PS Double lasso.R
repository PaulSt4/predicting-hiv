rm(list=ls())
load("./Data/project data.RData")

first_lasso_x <- model.matrix(HIVper100000 ~ ., D0)[, -1]
first_lasso_y <- scale(D0$HIVper100000)

first_lassoCV <- cv.glmnet(
  x = first_lasso_x,
  y = first_lasso_y,
  alpha = 1,
  nfolds = 10,
  standardize = FALSE
)

selected_1stlasso = coef(first_lassoCV, s = "lambda.1se") %>%
  broom:::tidy.dgCMatrix()
selected_1stlasso = selected_1stlasso[2:nrow(selected_1stlasso),1]

second_lasso_x_li <- model.matrix(Life.expectancy.at.birth..total..years. ~ ., D0)[, -1]
second_lasso_x_li <- second_lasso_x_li[ ,-1]
second_lasso_y_li <- scale(D0$Life.expectancy.at.birth..total..years.)


hiv_lassoCV_2nd <- cv.glmnet(
  x = second_lasso_x_li,
  y = second_lasso_y_li,
  alpha = 1,
  nfolds = 10,
  standardize = FALSE
)


selected_2ndlasso_li = coef(hiv_lassoCV_2nd_li, s = "lambda.1se") %>%
  broom:::tidy.dgCMatrix()
selected_2ndlasso_li = selected_2ndlasso_li[2:nrow(selected_2ndlasso_li),1]

Post_DL_OLS_li = lm(HIVper100000 ~ Life.expectancy.at.birth..total..years. + Unemployment..total....of.total.labor.force...modeled.ILO.estimate. + Sex.ratio.at.birth..male.births.per.female.births. + GDP.per.capita..PPP..current.international... + Rural.population....of.total.population. + Human.capital.index..HCI...scale.0.1. + Mortality.rate..infant..per.1.000.live.births. + Fertility.rate..total..births.per.woman. + Death.rate..crude..per.1.000.people. + Commercial.bank.branches..per.100.000.adults. + Mobile.cellular.subscriptions..per.100.people. + Female + External.debt.stocks....of.GNI. + People.using.at.least.basic.sanitation.services....of.population. + Prevalence.of.undernourishment....of.population., data = D0)
summary(Post_DL_OLS_li)

p_value_li = summary(Post_DL_OLS_li)$coef[,"Pr(>|t|)"]
p_value_li = p_value_li[2]

#*****************************************************************
#***************************************************************

second_lasso_x_un <- model.matrix(Unemployment..total....of.total.labor.force...modeled.ILO.estimate. ~ ., D0)[, -1]
second_lasso_x_un <- second_lasso_x_un[ ,-1]
second_lasso_y_un <- scale(D0$Unemployment..total....of.total.labor.force...modeled.ILO.estimate.)


hiv_lassoCV_2nd_un <- cv.glmnet(
  x = second_lasso_x_un,
  y = second_lasso_y_un,
  alpha = 1,
  nfolds = 10,
  standardize = FALSE
)

selected_2ndlasso_un = coef(hiv_lassoCV_2nd_un, s = "lambda.1se") %>%
  broom:::tidy.dgCMatrix()
selected_2ndlasso_un = selected_2ndlasso_un[2:nrow(selected_2ndlasso_un),1]

Post_DL_OLS_un = lm(HIVper100000 ~ Unemployment..total....of.total.labor.force...modeled.ILO.estimate. + Life.expectancy.at.birth..total..years. + Sex.ratio.at.birth..male.births.per.female.births. + Annual_precip + Female, data = D0)
summary(Post_DL_OLS_un)

p_value_un = summary(Post_DL_OLS_un)$coef[,"Pr(>|t|)"]
p_value_un = p_value_un[2]

#***************************************
  
second_lasso_x_sexr <- model.matrix(Sex.ratio.at.birth..male.births.per.female.births. ~ ., D0)[, -1]
second_lasso_x_sexr <- second_lasso_x_sexr[ ,-1]
second_lasso_y_sexr <- scale(D0$Sex.ratio.at.birth..male.births.per.female.births.)


hiv_lassoCV_2nd_sexr <- cv.glmnet(
  x = second_lasso_x_sexr,
  y = second_lasso_y_sexr,
  alpha = 1,
  nfolds = 10,
  standardize = FALSE
)

selected_2ndlasso_sexr = coef(hiv_lassoCV_2nd_sexr, s = "lambda.1se") %>%
  broom:::tidy.dgCMatrix()
selected_2ndlasso_sexr = selected_2ndlasso_sexr[2:nrow(selected_2ndlasso_sexr),1]

Post_DL_OLS_sexr = lm(HIVper100000 ~ Sex.ratio.at.birth..male.births.per.female.births. + Life.expectancy.at.birth..total..years. + Unemployment..total....of.total.labor.force...modeled.ILO.estimate. + Access.to.electricity....of.population. + Population.growth..annual... + Primary.completion.rate..total....of.relevant.age.group. + Prevalence.of.undernourishment....of.population., data = D0)

p_value_sexr = summary(Post_DL_OLS_sexr)$coef[,"Pr(>|t|)"]
p_value_sexr = p_value_sexr[2]

p_values = cbind(p_value_li, p_value_un, p_value_sexr)[,1:3]