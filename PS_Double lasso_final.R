# CAUSAL INFERENCE
# In this script we perform causal inference by applying the double selection method introduced by Belloni et al. (2014).
# It is based on performing LASSO-regression first on the variable we want to predict and then on then target variable, 
# which we want to investigate for causality. In a last step we perform OLS on the variable we are predicting using the 
# union of variables that remained after performing the LASSO-variable-regressions.

# We set our level of significance to 0.02 (certainty of 98%) since for a country it is important to know that if a indicator is determined 
# causally significant, that this is truly the case. Hence, Type 1 errors are more important to avoid than Type 2 errors

rm(list=ls())
load("../Data/project data.RData")

first_lasso_x <- model.matrix(HIVper100000 ~ (.)^2, D0)[, -1] # We set up a model matrix with all variables + interaction effects.
                                                              # This gives us a total of 741 variables.
lasso_data <- data.frame(first_lasso_x)
first_lasso_y <- scale(D0$HIVper100000) #scaling the y-values
Double_lasso_data = cbind(first_lasso_y, first_lasso_x)
Double_lasso_data = data.frame(Double_lasso_data) %>%
  rename(HIVper100000 = V1)



set.seed(123)
first_lassoCV <- cv.glmnet(   #cross-validated-lasso for the HIV-Values using all variables + interaction effects
  x = first_lasso_x,
  y = first_lasso_y,
  alpha = 1,
  nfolds = 10,
  standardize = FALSE
)

selected_1stlasso = coef(first_lassoCV, s = "lambda.1se") %>%
  broom:::tidy.dgCMatrix()
selected_1stlasso = selected_1stlasso[2:nrow(selected_1stlasso),1]

# Now we look at which values survived the LASSO-variable-reduction. The output is: 
# [1] "Life.expectancy.at.birth..total..years."                                                                       
# [2] "Unemployment..total....of.total.labor.force...modeled.ILO.estimate."                                           
# [3] "Access.to.electricity....of.population.:Prevalence.of.undernourishment....of.population."                      
# [4] "Life.expectancy.at.birth..total..years.:Unemployment..total....of.total.labor.force...modeled.ILO.estimate."   
# [5] "Life.expectancy.at.birth..total..years.:Sex.ratio.at.birth..male.births.per.female.births."                    
# [6] "Primary.completion.rate..total....of.relevant.age.group.:Legislation.exists.on.domestic.violence..1.yes..0.no."
# [7] "Male:Prevalence.of.undernourishment....of.population."  
# We take those 7 variables and run a LASSO-regression on each of them using the other variables

second_lasso_x_li <- model.matrix(Life.expectancy.at.birth..total..years. ~ ., lasso_data)[, -1]
second_lasso_y_li <- scale(D0$Life.expectancy.at.birth..total..years.)

set.seed(123)
hiv_lassoCV_2nd_li <- cv.glmnet(
  x = second_lasso_x_li,
  y = second_lasso_y_li,
  alpha = 1,
  nfolds = 10,
  standardize = FALSE
)


selected_2ndlasso_li = coef(hiv_lassoCV_2nd_li, s = "lambda.1se") %>%
  broom:::tidy.dgCMatrix()
selected_2ndlasso_li = selected_2ndlasso_li[2:nrow(selected_2ndlasso_li),1]

# These variables survived the lasso for Life Expectancy:
# [1] "GDP.per.capita..PPP..current.international..."                                                                       
# [2] "Rural.population....of.total.population."                                                                            
# [3] "Human.capital.index..HCI...scale.0.1."                                                                               
# [4] "Access.to.electricity....of.population."                                                                             
# [5] "Mortality.rate..infant..per.1.000.live.births."                                                                      
# [6] "Fertility.rate..total..births.per.woman."                                                                            
# [7] "Death.rate..crude..per.1.000.people."                                                                                
# [8] "People.using.at.least.basic.sanitation.services....of.population."                                                   
# [9] "Rural.population....of.total.population..Military.expenditure....of.general.government.expenditure."                 
# [10] "Consumer.price.index..2010...100..Inflation..consumer.prices..annual..."                                             
# [11] "Human.capital.index..HCI...scale.0.1..Adjusted.net.enrollment.rate..primary....of.primary.school.age.children."      
# [12] "Death.rate..crude..per.1.000.people..Life.expectancy.at.birth..total..years."                                        
# [13] "Life.expectancy.at.birth..total..years..Unemployment..total....of.total.labor.force...modeled.ILO.estimate."         
# [14] "GDP.growth..annual....Primary.completion.rate..total....of.relevant.age.group."                                      
# [15] "Primary.completion.rate..total....of.relevant.age.group..Legislation.exists.on.domestic.violence..1.yes..0.no."      
# [16] "Annual_temp.External.debt.stocks....of.GNI."                                                                         
# [17] "Sex.ratio.at.birth..male.births.per.female.births..People.using.at.least.basic.sanitation.services....of.population."
# [18] "Sex.ratio.at.birth..male.births.per.female.births..Prevalence.of.undernourishment....of.population."                 
# [19] "Legislation.exists.on.domestic.violence..1.yes..0.no..Prevalence.of.undernourishment....of.population."

# We now perform a Linear regression on the union of the variables which survived the first and the second LASSO.

Post_DL_OLS_li = lm(HIVper100000 ~ Life.expectancy.at.birth..total..years. +                                                                      
                    Unemployment..total....of.total.labor.force...modeled.ILO.estimate. +                                           
                    Access.to.electricity....of.population..Prevalence.of.undernourishment....of.population. +                      
                    Life.expectancy.at.birth..total..years..Unemployment..total....of.total.labor.force...modeled.ILO.estimate. +   
                    Life.expectancy.at.birth..total..years..Sex.ratio.at.birth..male.births.per.female.births. +                    
                    Primary.completion.rate..total....of.relevant.age.group..Legislation.exists.on.domestic.violence..1.yes..0.no. +
                    Male.Prevalence.of.undernourishment....of.population. + 
                    GDP.per.capita..PPP..current.international... +                                                                       
                    Rural.population....of.total.population. +                                                                            
                    Human.capital.index..HCI...scale.0.1. +                                                                               
                    Access.to.electricity....of.population. +                                                                             
                    Mortality.rate..infant..per.1.000.live.births. +                                                                      
                    Fertility.rate..total..births.per.woman. +                                                                            
                    Death.rate..crude..per.1.000.people. +                                                                                
                    People.using.at.least.basic.sanitation.services....of.population. +                                                   
                    Rural.population....of.total.population..Military.expenditure....of.general.government.expenditure. +                 
                    Consumer.price.index..2010...100..Inflation..consumer.prices..annual... +                                             
                    Human.capital.index..HCI...scale.0.1..Adjusted.net.enrollment.rate..primary....of.primary.school.age.children. +      
                    Death.rate..crude..per.1.000.people..Life.expectancy.at.birth..total..years. +                                        
                    Life.expectancy.at.birth..total..years..Unemployment..total....of.total.labor.force...modeled.ILO.estimate. +         
                    GDP.growth..annual....Primary.completion.rate..total....of.relevant.age.group. +                                      
                    Primary.completion.rate..total....of.relevant.age.group..Legislation.exists.on.domestic.violence..1.yes..0.no. +      
                    Annual_temp.External.debt.stocks....of.GNI. +                                                                         
                    Sex.ratio.at.birth..male.births.per.female.births..People.using.at.least.basic.sanitation.services....of.population. +
                    Sex.ratio.at.birth..male.births.per.female.births..Prevalence.of.undernourishment....of.population. +                 
                    Legislation.exists.on.domestic.violence..1.yes..0.no..Prevalence.of.undernourishment....of.population., data = Double_lasso_data)
                    
                    
                    
summary(Post_DL_OLS_li)

p_value_li = summary(Post_DL_OLS_li)$coef[,"Pr(>|t|)"]
p_value_li = p_value_li[2]

# The resulting p-value for Life expectancy at birth is 0.0001818. This value is significant.

#************REPEATING THE STEPS FOR THE OTHER VARIABLES


second_lasso_x_ur <- model.matrix(Unemployment..total....of.total.labor.force...modeled.ILO.estimate. ~ ., lasso_data)[, -1]
second_lasso_y_ur <- lasso_data$Unemployment..total....of.total.labor.force...modeled.ILO.estimate.

set.seed(123)
hiv_lassoCV_2nd_ur <- cv.glmnet(
  x = second_lasso_x_ur,
  y = second_lasso_y_ur,
  alpha = 1,
  nfolds = 10,
  standardize = FALSE
)

selected_2ndlasso_ur = coef(hiv_lassoCV_2nd_ur, s = "lambda.1se") %>%
  broom:::tidy.dgCMatrix()
selected_2ndlasso_ur = selected_2ndlasso_ur[2:nrow(selected_2ndlasso_ur),1]

# The variables that survived are:
# [1] "Death.rate..crude..per.1.000.people."                                                                             
# [2] "Commercial.bank.branches..per.100.000.adults."                                                                    
# [3] "Annual_precip"                                                                                                    
# [4] "Final.consumption.expenditure....of.GDP."                                                                         
# [5] "GDP.per.capita..PPP..current.international....Rural.population....of.total.population."                           
# [6] "GDP.per.capita..PPP..current.international....Unemployment..total....of.total.labor.force...modeled.ILO.estimate."
# [7] "Rural.population....of.total.population..Both.sexes"                                                              
# [8] "Consumer.price.index..2010...100..Inflation..consumer.prices..annual..."                                          
# [9] "Death.rate..crude..per.1.000.people..Unemployment..total....of.total.labor.force...modeled.ILO.estimate."         
# [10] "Life.expectancy.at.birth..total..years..Unemployment..total....of.total.labor.force...modeled.ILO.estimate."      
# [11] "GDP.growth..annual....Annual_precip"                                                                              
# [12] "GDP.growth..annual....Prevalence.of.undernourishment....of.population."                                           
# [13] "Unemployment..total....of.total.labor.force...modeled.ILO.estimate..Expense....of.GDP."                           
# [14] "Unemployment..total....of.total.labor.force...modeled.ILO.estimate..Annual_precip"                                
# [15] "Unemployment..total....of.total.labor.force...modeled.ILO.estimate..Subsidies.and.other.transfers....of.expense." 
# [16] "Primary.completion.rate..total....of.relevant.age.group..Final.consumption.expenditure....of.GDP."                
# [17] "Annual_temp.Sex.ratio.at.birth..male.births.per.female.births."                                                   
# [18] "Male.Female"                                                                                                      
# [19] "Sex.ratio.at.birth..male.births.per.female.births..External.debt.stocks....of.GNI."

Post_DL_OLS_ur = lm(HIVper100000 ~ Life.expectancy.at.birth..total..years. +                                                                      
                      Unemployment..total....of.total.labor.force...modeled.ILO.estimate. +                                           
                      Access.to.electricity....of.population..Prevalence.of.undernourishment....of.population. +                      
                      Life.expectancy.at.birth..total..years..Unemployment..total....of.total.labor.force...modeled.ILO.estimate. +   
                      Life.expectancy.at.birth..total..years..Sex.ratio.at.birth..male.births.per.female.births. +                    
                      Primary.completion.rate..total....of.relevant.age.group..Legislation.exists.on.domestic.violence..1.yes..0.no. +
                      Male.Prevalence.of.undernourishment....of.population. + 
                      
                      Death.rate..crude..per.1.000.people. +                                                                             
                      Commercial.bank.branches..per.100.000.adults. +                                                                    
                      Annual_precip +                                                                                                    
                      Final.consumption.expenditure....of.GDP. +                                                                         
                      GDP.per.capita..PPP..current.international....Rural.population....of.total.population. +                           
                      GDP.per.capita..PPP..current.international....Unemployment..total....of.total.labor.force...modeled.ILO.estimate. +
                      Rural.population....of.total.population..Both.sexes +                                                              
                      Consumer.price.index..2010...100..Inflation..consumer.prices..annual... +                                          
                      Death.rate..crude..per.1.000.people..Unemployment..total....of.total.labor.force...modeled.ILO.estimate. +         
                      GDP.growth..annual....Annual_precip +                                                                              
                      GDP.growth..annual....Prevalence.of.undernourishment....of.population. +                                           
                      Unemployment..total....of.total.labor.force...modeled.ILO.estimate..Expense....of.GDP. +                          
                      Unemployment..total....of.total.labor.force...modeled.ILO.estimate..Annual_precip +                                
                      Unemployment..total....of.total.labor.force...modeled.ILO.estimate..Subsidies.and.other.transfers....of.expense. + 
                      Primary.completion.rate..total....of.relevant.age.group..Final.consumption.expenditure....of.GDP. +                
                      Annual_temp.Sex.ratio.at.birth..male.births.per.female.births. +                                                   
                      Male.Female +                                                                                                      
                      Sex.ratio.at.birth..male.births.per.female.births..External.debt.stocks....of.GNI., data = Double_lasso_data)
                    
                      
                      
summary(Post_DL_OLS_ur)

p_value_ur = summary(Post_DL_OLS_ur)$coef[,"Pr(>|t|)"]
p_value_ur = p_value_ur[3]

# The p-value of the unemployment rate is significant 
# for the HIV-rate, since it has a value of 0.00003225968.

#***************************************
  
second_lasso_x_eu <- model.matrix(Access.to.electricity....of.population..Prevalence.of.undernourishment....of.population.  ~ ., lasso_data)[, -1]
second_lasso_y_eu <- lasso_data$Access.to.electricity....of.population..Prevalence.of.undernourishment....of.population.

set.seed(123)
hiv_lassoCV_2nd_eu <- cv.glmnet(
  x = second_lasso_x_eu,
  y = second_lasso_y_eu,
  alpha = 1,
  nfolds = 10,
  standardize = FALSE
)

selected_2ndlasso_eu = coef(hiv_lassoCV_2nd_eu, s = "lambda.1se") %>%
  broom:::tidy.dgCMatrix()
selected_2ndlasso_eu = selected_2ndlasso_eu[2:nrow(selected_2ndlasso_eu),1]

# The variables which survived the second lasso are:
# [1] "Prevalence.of.undernourishment....of.population."                                                                  
# [2] "Rural.population....of.total.population..Access.to.electricity....of.population."                                  
# [3] "Consumer.price.index..2010...100..Inflation..consumer.prices..annual..."                                           
# [4] "Human.capital.index..HCI...scale.0.1..Access.to.electricity....of.population."                                     
# [5] "Access.to.electricity....of.population..Mortality.rate..infant..per.1.000.live.births."                            
# [6] "Access.to.electricity....of.population..Age.dependency.ratio....of.working.age.population."                        
# [7] "Access.to.electricity....of.population..Fertility.rate..total..births.per.woman."                                  
# [8] "Access.to.electricity....of.population..Mobile.cellular.subscriptions..per.100.people."                            
# [9] "Mortality.rate..under.5..per.1.000.live.births..Fertility.rate..total..births.per.woman."                          
# [10] "Mortality.rate..infant..per.1.000.live.births..Prevalence.of.undernourishment....of.population."                   
# [11] "Age.dependency.ratio....of.working.age.population..Legislation.exists.on.domestic.violence..1.yes..0.no."          
# [12] "Age.dependency.ratio....of.working.age.population..Prevalence.of.undernourishment....of.population."               
# [13] "Fertility.rate..total..births.per.woman..Legislation.exists.on.domestic.violence..1.yes..0.no."                    
# [14] "Fertility.rate..total..births.per.woman..Prevalence.of.undernourishment....of.population."                         
# [15] "Life.expectancy.at.birth..total..years..Prevalence.of.undernourishment....of.population."                          
# [16] "GDP.growth..annual....Expense....of.GDP."                                                                          
# [17] "GDP.growth..annual....Prevalence.of.undernourishment....of.population."                                            
# [18] "Adjusted.net.enrollment.rate..primary....of.primary.school.age.children..Female"                                   
# [19] "Expense....of.GDP..Prevalence.of.undernourishment....of.population."                                               
# [20] "Mobile.cellular.subscriptions..per.100.people..Male"                                                               
# [21] "Annual_precip.Legislation.exists.on.domestic.violence..1.yes..0.no."                                               
# [22] "Annual_precip.People.using.at.least.basic.sanitation.services....of.population."                                   
# [23] "Male.Legislation.exists.on.domestic.violence..1.yes..0.no."                                                        
# [24] "Male.People.using.at.least.basic.sanitation.services....of.population."                                            
# [25] "Female.Prevalence.of.undernourishment....of.population."                                                           
# [26] "Sex.ratio.at.birth..male.births.per.female.births..Prevalence.of.undernourishment....of.population."               
# [27] "Final.consumption.expenditure....of.GDP..Prevalence.of.undernourishment....of.population."                         
# [28] "Final.consumption.expenditure....of.GDP..Subsidies.and.other.transfers....of.expense."                             
# [29] "Legislation.exists.on.domestic.violence..1.yes..0.no..Subsidies.and.other.transfers....of.expense."                
# [30] "People.using.at.least.basic.sanitation.services....of.population..Prevalence.of.undernourishment....of.population."
# [31] "People.using.at.least.basic.sanitation.services....of.population..Subsidies.and.other.transfers....of.expense."

Post_DL_OLS_eu = lm(HIVper100000 ~ Life.expectancy.at.birth..total..years. +                                                                      
                      Unemployment..total....of.total.labor.force...modeled.ILO.estimate. +                                           
                      Access.to.electricity....of.population.:Prevalence.of.undernourishment....of.population. +                      
                      Life.expectancy.at.birth..total..years.:Unemployment..total....of.total.labor.force...modeled.ILO.estimate. +   
                      Life.expectancy.at.birth..total..years.:Sex.ratio.at.birth..male.births.per.female.births. +                    
                      Primary.completion.rate..total....of.relevant.age.group.:Legislation.exists.on.domestic.violence..1.yes..0.no. +
                      Male:Prevalence.of.undernourishment....of.population. + 
                      
                    Prevalence.of.undernourishment....of.population. +                                                                  
                    Rural.population....of.total.population..Access.to.electricity....of.population. +                                  
                    Consumer.price.index..2010...100..Inflation..consumer.prices..annual... +                                           
                    Human.capital.index..HCI...scale.0.1..Access.to.electricity....of.population. +                                    
                    Access.to.electricity....of.population..Mortality.rate..infant..per.1.000.live.births. +                            
                    Access.to.electricity....of.population..Age.dependency.ratio....of.working.age.population. +                        
                    Access.to.electricity....of.population..Fertility.rate..total..births.per.woman. +                                  
                    Access.to.electricity....of.population..Mobile.cellular.subscriptions..per.100.people. +                            
                    Mortality.rate..under.5..per.1.000.live.births..Fertility.rate..total..births.per.woman. +                          
                    Mortality.rate..infant..per.1.000.live.births..Prevalence.of.undernourishment....of.population. +                   
                    Age.dependency.ratio....of.working.age.population..Legislation.exists.on.domestic.violence..1.yes..0.no. +          
                    Age.dependency.ratio....of.working.age.population..Prevalence.of.undernourishment....of.population. +               
                    Fertility.rate..total..births.per.woman..Legislation.exists.on.domestic.violence..1.yes..0.no. +                    
                    Fertility.rate..total..births.per.woman..Prevalence.of.undernourishment....of.population. +                         
                    Life.expectancy.at.birth..total..years..Prevalence.of.undernourishment....of.population. +                          
                    GDP.growth..annual....Expense....of.GDP. +                                                                          
                    GDP.growth..annual....Prevalence.of.undernourishment....of.population. +                                            
                    Adjusted.net.enrollment.rate..primary....of.primary.school.age.children..Female +                                   
                    Expense....of.GDP..Prevalence.of.undernourishment....of.population. +                                               
                    Mobile.cellular.subscriptions..per.100.people..Male +                                                               
                    Annual_precip.Legislation.exists.on.domestic.violence..1.yes..0.no. +                                               
                    Annual_precip.People.using.at.least.basic.sanitation.services....of.population. +                                   
                    Male.Legislation.exists.on.domestic.violence..1.yes..0.no. +                                                        
                    Male.People.using.at.least.basic.sanitation.services....of.population. +                                            
                    Female.Prevalence.of.undernourishment....of.population. +                                                           
                    Sex.ratio.at.birth..male.births.per.female.births..Prevalence.of.undernourishment....of.population. +               
                    Final.consumption.expenditure....of.GDP..Prevalence.of.undernourishment....of.population. +                         
                    Final.consumption.expenditure....of.GDP..Subsidies.and.other.transfers....of.expense. +                             
                    Legislation.exists.on.domestic.violence..1.yes..0.no..Subsidies.and.other.transfers....of.expense. +                
                    People.using.at.least.basic.sanitation.services....of.population..Prevalence.of.undernourishment....of.population. +
                    People.using.at.least.basic.sanitation.services....of.population..Subsidies.and.other.transfers....of.expense., data = Double_lasso_data)

p_value_eu = summary(Post_DL_OLS_eu)$coef[,"Pr(>|t|)"]
p_value_eu = p_value_eu[35]

# The interaction between access to electricity and undernourishment gives us a p-value of 0.03067 with regards to the HIV-values. Hence, the interaction is not significant since we 
# use 0.02 as our level of significance.

#*************************************************

second_lasso_x_lu <- model.matrix(Life.expectancy.at.birth..total..years..Unemployment..total....of.total.labor.force...modeled.ILO.estimate. ~ ., lasso_data)[, -1]
second_lasso_y_lu <- lasso_data$Life.expectancy.at.birth..total..years..Unemployment..total....of.total.labor.force...modeled.ILO.estimate.

set.seed(123)
hiv_lassoCV_2nd_lu <- cv.glmnet(
  x = second_lasso_x_lu,
  y = second_lasso_y_lu,
  alpha = 1,
  nfolds = 10,
  standardize = FALSE
)

selected_2ndlasso_lu = coef(hiv_lassoCV_2nd_lu, s = "lambda.1se") %>%
  broom:::tidy.dgCMatrix()
selected_2ndlasso_lu = selected_2ndlasso_lu[2:nrow(selected_2ndlasso_lu),1]

# The variables which survived the second lasso are:
# [1] "Unemployment..total....of.total.labor.force...modeled.ILO.estimate."                                                                  
# [2] "Consumer.price.index..2010...100..Inflation..consumer.prices..annual..."                                                              
# [3] "Human.capital.index..HCI...scale.0.1..Unemployment..total....of.total.labor.force...modeled.ILO.estimate."                            
# [4] "Mortality.rate..infant..per.1.000.live.births..Unemployment..total....of.total.labor.force...modeled.ILO.estimate."                   
# [5] "Death.rate..crude..per.1.000.people..Unemployment..total....of.total.labor.force...modeled.ILO.estimate."                             
# [6] "GDP.growth..annual....Military.expenditure....of.general.government.expenditure."                                                     
# [7] "Unemployment..total....of.total.labor.force...modeled.ILO.estimate..Female"                                                           
# [8] "Unemployment..total....of.total.labor.force...modeled.ILO.estimate..Sex.ratio.at.birth..male.births.per.female.births."               
# [9] "Unemployment..total....of.total.labor.force...modeled.ILO.estimate..People.using.at.least.basic.sanitation.services....of.population."
# [10] "Adjusted.net.enrollment.rate..primary....of.primary.school.age.children..Final.consumption.expenditure....of.GDP."

Post_DL_OLS_lu = lm(HIVper100000 ~ Life.expectancy.at.birth..total..years. +                                                                      
                      Unemployment..total....of.total.labor.force...modeled.ILO.estimate. +                                           
                      Access.to.electricity....of.population.:Prevalence.of.undernourishment....of.population. +                      
                      Life.expectancy.at.birth..total..years.:Unemployment..total....of.total.labor.force...modeled.ILO.estimate. +   
                      Life.expectancy.at.birth..total..years.:Sex.ratio.at.birth..male.births.per.female.births. +                    
                      Primary.completion.rate..total....of.relevant.age.group.:Legislation.exists.on.domestic.violence..1.yes..0.no. +
                      Male:Prevalence.of.undernourishment....of.population. + 
                      
                      Unemployment..total....of.total.labor.force...modeled.ILO.estimate. +                                                                 
                      Consumer.price.index..2010...100..Inflation..consumer.prices..annual... +                                                              
                      Human.capital.index..HCI...scale.0.1..Unemployment..total....of.total.labor.force...modeled.ILO.estimate. +                            
                      Mortality.rate..infant..per.1.000.live.births..Unemployment..total....of.total.labor.force...modeled.ILO.estimate. +                   
                      Death.rate..crude..per.1.000.people..Unemployment..total....of.total.labor.force...modeled.ILO.estimate. +                             
                      GDP.growth..annual....Military.expenditure....of.general.government.expenditure. +                                                     
                      Unemployment..total....of.total.labor.force...modeled.ILO.estimate..Female  +                                                         
                      Unemployment..total....of.total.labor.force...modeled.ILO.estimate..Sex.ratio.at.birth..male.births.per.female.births.  +             
                      Unemployment..total....of.total.labor.force...modeled.ILO.estimate..People.using.at.least.basic.sanitation.services....of.population. +
                      Adjusted.net.enrollment.rate..primary....of.primary.school.age.children..Final.consumption.expenditure....of.GDP., data = Double_lasso_data)
                    
                      
p_value_lu = summary(Post_DL_OLS_lu)$coef[,"Pr(>|t|)"]
p_value_lu = p_value_lu[14]

# We obtain a p-value of 0.0052 from the interaction effect of life expectancy and total unemployment with regards to the HIV-rate. Hence, the impact of the 
# interaction effect is significant. This is expectable since both main effects are significant.

#**********************************************

second_lasso_x_ls <- model.matrix(Life.expectancy.at.birth..total..years..Sex.ratio.at.birth..male.births.per.female.births. ~ ., lasso_data)[, -1]
second_lasso_y_ls <- lasso_data$Life.expectancy.at.birth..total..years..Sex.ratio.at.birth..male.births.per.female.births.

set.seed(123)
hiv_lassoCV_2nd_ls <- cv.glmnet(
  x = second_lasso_x_ls,
  y = second_lasso_y_ls,
  alpha = 1,
  nfolds = 10,
  standardize = FALSE
)

selected_2ndlasso_ls = coef(hiv_lassoCV_2nd_ls, s = "lambda.1se") %>%
  broom:::tidy.dgCMatrix()
selected_2ndlasso_ls = selected_2ndlasso_ls[2:nrow(selected_2ndlasso_ls),1]

# The variables which survived the second lasso are:
# [1] "Unemployment..total....of.total.labor.force...modeled.ILO.estimate."                                                 
# [2] "Sex.ratio.at.birth..male.births.per.female.births."                                                                  
# [3] "GDP.per.capita..PPP..current.international....Sex.ratio.at.birth..male.births.per.female.births."                    
# [4] "Consumer.price.index..2010...100..Inflation..consumer.prices..annual..."                                             
# [5] "Human.capital.index..HCI...scale.0.1..Life.expectancy.at.birth..total..years."                                       
# [6] "Human.capital.index..HCI...scale.0.1..Sex.ratio.at.birth..male.births.per.female.births."                            
# [7] "Mortality.rate..under.5..per.1.000.live.births..Fertility.rate..total..births.per.woman."                            
# [8] "Mortality.rate..under.5..per.1.000.live.births..Sex.ratio.at.birth..male.births.per.female.births."                  
# [9] "Mortality.rate..infant..per.1.000.live.births..Sex.ratio.at.birth..male.births.per.female.births."                   
# [10] "Mortality.rate..infant..per.1.000.live.births..Export.volume.index..2000...100."                                     
# [11] "Fertility.rate..total..births.per.woman..Sex.ratio.at.birth..male.births.per.female.births."                         
# [12] "Death.rate..crude..per.1.000.people..Unemployment..total....of.total.labor.force...modeled.ILO.estimate."            
# [13] "Death.rate..crude..per.1.000.people..Sex.ratio.at.birth..male.births.per.female.births."                             
# [14] "Life.expectancy.at.birth..total..years..Unemployment..total....of.total.labor.force...modeled.ILO.estimate."         
# [15] "Life.expectancy.at.birth..total..years..Prevalence.of.undernourishment....of.population."                            
# [16] "GDP.growth..annual....Unemployment..total....of.total.labor.force...modeled.ILO.estimate."                           
# [17] "GDP.growth..annual....Legislation.exists.on.domestic.violence..1.yes..0.no."                                         
# [18] "Commercial.bank.branches..per.100.000.adults..External.debt.stocks....of.GNI."                                       
# [19] "Sex.ratio.at.birth..male.births.per.female.births..External.debt.stocks....of.GNI."                                  
# [20] "Sex.ratio.at.birth..male.births.per.female.births..People.using.at.least.basic.sanitation.services....of.population."
# [21] "Legislation.exists.on.domestic.violence..1.yes..0.no..Prevalence.of.undernourishment....of.population."

Post_DL_OLS_ls = lm(HIVper100000 ~ Life.expectancy.at.birth..total..years. +                                                                      
                      Unemployment..total....of.total.labor.force...modeled.ILO.estimate. +                                           
                      Access.to.electricity....of.population.:Prevalence.of.undernourishment....of.population. +                      
                      Life.expectancy.at.birth..total..years.:Unemployment..total....of.total.labor.force...modeled.ILO.estimate. +   
                      Life.expectancy.at.birth..total..years.:Sex.ratio.at.birth..male.births.per.female.births. +                    
                      Primary.completion.rate..total....of.relevant.age.group.:Legislation.exists.on.domestic.violence..1.yes..0.no. +
                      Male:Prevalence.of.undernourishment....of.population. + 
                      
                      Unemployment..total....of.total.labor.force...modeled.ILO.estimate. +                                                 
                      Sex.ratio.at.birth..male.births.per.female.births. +                                                                  
                      GDP.per.capita..PPP..current.international....Sex.ratio.at.birth..male.births.per.female.births. +                    
                      Consumer.price.index..2010...100..Inflation..consumer.prices..annual... +                                             
                      Human.capital.index..HCI...scale.0.1..Life.expectancy.at.birth..total..years. +                                       
                      Human.capital.index..HCI...scale.0.1..Sex.ratio.at.birth..male.births.per.female.births. +                            
                      Mortality.rate..under.5..per.1.000.live.births..Fertility.rate..total..births.per.woman. +                            
                      Mortality.rate..under.5..per.1.000.live.births..Sex.ratio.at.birth..male.births.per.female.births. +                  
                      Mortality.rate..infant..per.1.000.live.births..Sex.ratio.at.birth..male.births.per.female.births. +                   
                      Mortality.rate..infant..per.1.000.live.births..Export.volume.index..2000...100. +                                     
                      Fertility.rate..total..births.per.woman..Sex.ratio.at.birth..male.births.per.female.births. +                         
                      Death.rate..crude..per.1.000.people..Unemployment..total....of.total.labor.force...modeled.ILO.estimate. +            
                      Death.rate..crude..per.1.000.people..Sex.ratio.at.birth..male.births.per.female.births. +                             
                      Life.expectancy.at.birth..total..years..Unemployment..total....of.total.labor.force...modeled.ILO.estimate. +         
                      Life.expectancy.at.birth..total..years..Prevalence.of.undernourishment....of.population. +                            
                      GDP.growth..annual....Unemployment..total....of.total.labor.force...modeled.ILO.estimate. +                           
                      GDP.growth..annual....Legislation.exists.on.domestic.violence..1.yes..0.no. +                                         
                      Commercial.bank.branches..per.100.000.adults..External.debt.stocks....of.GNI. +                                       
                      Sex.ratio.at.birth..male.births.per.female.births..External.debt.stocks....of.GNI. +                                  
                      Sex.ratio.at.birth..male.births.per.female.births..People.using.at.least.basic.sanitation.services....of.population. +
                      Legislation.exists.on.domestic.violence..1.yes..0.no..Prevalence.of.undernourishment....of.population., data = Double_lasso_data)
                    

p_value_ls = summary(Post_DL_OLS_ls)$coef[,"Pr(>|t|)"]
p_value_ls = p_value_ls[25]

# With a p-value of 0.010885 the Interaction between Life Expectancy and the sex ratio at birth is causally significant.

#****************************************

second_lasso_x_pl <- model.matrix(Primary.completion.rate..total....of.relevant.age.group..Legislation.exists.on.domestic.violence..1.yes..0.no. ~ ., lasso_data)[, -1]
second_lasso_y_pl <- lasso_data$Primary.completion.rate..total....of.relevant.age.group..Legislation.exists.on.domestic.violence..1.yes..0.no.

set.seed(123)
hiv_lassoCV_2nd_pl <- cv.glmnet(
  x = second_lasso_x_pl,
  y = second_lasso_y_pl,
  alpha = 1,
  nfolds = 10,
  standardize = FALSE
)

selected_2ndlasso_pl = coef(hiv_lassoCV_2nd_pl, s = "lambda.1se") %>%
  broom:::tidy.dgCMatrix()
selected_2ndlasso_pl = selected_2ndlasso_pl[2:nrow(selected_2ndlasso_pl),1]

# The variables which survived the second lasso are:
# [1] "Consumer.price.index..2010...100..Inflation..consumer.prices..annual..."                                                       
# [2] "Mortality.rate..under.5..per.1.000.live.births..Legislation.exists.on.domestic.violence..1.yes..0.no."                         
# [3] "Age.dependency.ratio....of.working.age.population..Legislation.exists.on.domestic.violence..1.yes..0.no."                      
# [4] "Fertility.rate..total..births.per.woman..Legislation.exists.on.domestic.violence..1.yes..0.no."                                
# [5] "Adjusted.net.enrollment.rate..primary....of.primary.school.age.children..Legislation.exists.on.domestic.violence..1.yes..0.no."

Post_DL_OLS_pl = lm(HIVper100000 ~ Life.expectancy.at.birth..total..years. +                                                                      
                      Unemployment..total....of.total.labor.force...modeled.ILO.estimate. +                                           
                      Access.to.electricity....of.population.:Prevalence.of.undernourishment....of.population. +                      
                      Life.expectancy.at.birth..total..years.:Unemployment..total....of.total.labor.force...modeled.ILO.estimate. +   
                      Life.expectancy.at.birth..total..years.:Sex.ratio.at.birth..male.births.per.female.births. +                    
                      Primary.completion.rate..total....of.relevant.age.group.:Legislation.exists.on.domestic.violence..1.yes..0.no. +
                      Male:Prevalence.of.undernourishment....of.population. + 
                      
                      Consumer.price.index..2010...100..Inflation..consumer.prices..annual... +                                                       
                      Mortality.rate..under.5..per.1.000.live.births..Legislation.exists.on.domestic.violence..1.yes..0.no. +                         
                      Age.dependency.ratio....of.working.age.population..Legislation.exists.on.domestic.violence..1.yes..0.no. +                     
                      Fertility.rate..total..births.per.woman..Legislation.exists.on.domestic.violence..1.yes..0.no. +                                
                      Adjusted.net.enrollment.rate..primary....of.primary.school.age.children..Legislation.exists.on.domestic.violence..1.yes..0.no., data = Double_lasso_data)
                      

p_value_pl = summary(Post_DL_OLS_pl)$coef[,"Pr(>|t|)"]
p_value_pl = p_value_pl[12]

# With a p-value of 0.02656 the interaction between the primary completion rate and legislation on domestic violence is causally not significant
# with a level of significance of 0.02.

#************************************************

second_lasso_x_mu <- model.matrix(Male.Prevalence.of.undernourishment....of.population. ~ ., lasso_data)[, -1]
second_lasso_y_mu <- lasso_data$Male.Prevalence.of.undernourishment....of.population.

set.seed(123)
hiv_lassoCV_2nd_mu <- cv.glmnet(
  x = second_lasso_x_mu,
  y = second_lasso_y_mu,
  alpha = 1,
  nfolds = 10,
  standardize = FALSE
)

selected_2ndlasso_mu = coef(hiv_lassoCV_2nd_mu, s = "lambda.1se") %>%
  broom:::tidy.dgCMatrix()
selected_2ndlasso_mu = selected_2ndlasso_mu[2:nrow(selected_2ndlasso_mu),1]

# The variables which survived the second lasso are:
# [1] "Consumer.price.index..2010...100..Inflation..consumer.prices..annual..."                                                  
# [2] "Human.capital.index..HCI...scale.0.1..Prevalence.of.undernourishment....of.population."                                   
# [3] "Access.to.electricity....of.population..Mortality.rate..infant..per.1.000.live.births."                                   
# [4] "Access.to.electricity....of.population..Death.rate..crude..per.1.000.people."                                             
# [5] "Access.to.electricity....of.population..Adjusted.net.enrollment.rate..primary....of.primary.school.age.children."         
# [6] "Access.to.electricity....of.population..Mobile.cellular.subscriptions..per.100.people."                                   
# [7] "Access.to.electricity....of.population..Male"                                                                             
# [8] "Access.to.electricity....of.population..Female"                                                                           
# [9] "Mortality.rate..infant..per.1.000.live.births..Male"                                                                      
# [10] "Age.dependency.ratio....of.working.age.population..Male"                                                                  
# [11] "Age.dependency.ratio....of.working.age.population..Prevalence.of.undernourishment....of.population."                      
# [12] "Death.rate..crude..per.1.000.people..Prevalence.of.undernourishment....of.population."                                    
# [13] "Life.expectancy.at.birth..total..years..Prevalence.of.undernourishment....of.population."                                 
# [14] "GDP.growth..annual....Legislation.exists.on.domestic.violence..1.yes..0.no."                                              
# [15] "Unemployment..total....of.total.labor.force...modeled.ILO.estimate..Subsidies.and.other.transfers....of.expense."         
# [16] "Adjusted.net.enrollment.rate..primary....of.primary.school.age.children..Final.consumption.expenditure....of.GDP."        
# [17] "Adjusted.net.enrollment.rate..primary....of.primary.school.age.children..Prevalence.of.undernourishment....of.population."
# [18] "Expense....of.GDP..Prevalence.of.undernourishment....of.population."                                                      
# [19] "Military.expenditure....of.general.government.expenditure..Prevalence.of.undernourishment....of.population."              
# [20] "Annual_precip.Prevalence.of.undernourishment....of.population."                                                           
# [21] "Both.sexes.Prevalence.of.undernourishment....of.population."                                                              
# [22] "Female.People.using.at.least.basic.sanitation.services....of.population." 
# [23] "Female.Prevalence.of.undernourishment....of.population."
# [23] "Sex.ratio.at.birth..male.births.per.female.births..Prevalence.of.undernourishment....of.population."                      
# [24] "Legislation.exists.on.domestic.violence..1.yes..0.no..Prevalence.of.undernourishment....of.population."                   
# [25] "Prevalence.of.undernourishment....of.population..Subsidies.and.other.transfers....of.expense." 

Post_DL_OLS_mu = lm(HIVper100000 ~ Life.expectancy.at.birth..total..years. +                                                                      
                      Unemployment..total....of.total.labor.force...modeled.ILO.estimate. +                                           
                      Access.to.electricity....of.population.:Prevalence.of.undernourishment....of.population. +                      
                      Life.expectancy.at.birth..total..years.:Unemployment..total....of.total.labor.force...modeled.ILO.estimate. +   
                      Life.expectancy.at.birth..total..years.:Sex.ratio.at.birth..male.births.per.female.births. +                    
                      Primary.completion.rate..total....of.relevant.age.group.:Legislation.exists.on.domestic.violence..1.yes..0.no. +
                      Male:Prevalence.of.undernourishment....of.population. + 
                      
                      Consumer.price.index..2010...100..Inflation..consumer.prices..annual... +                                                  
                      Human.capital.index..HCI...scale.0.1..Prevalence.of.undernourishment....of.population. +                                   
                      Access.to.electricity....of.population..Mortality.rate..infant..per.1.000.live.births. +                                   
                      Access.to.electricity....of.population..Death.rate..crude..per.1.000.people. +                                             
                      Access.to.electricity....of.population..Adjusted.net.enrollment.rate..primary....of.primary.school.age.children. +         
                      Access.to.electricity....of.population..Mobile.cellular.subscriptions..per.100.people. +                                   
                      Access.to.electricity....of.population..Male +                                                                             
                      Access.to.electricity....of.population..Female +                                                                           
                      Mortality.rate..infant..per.1.000.live.births..Male +                                                                      
                      Age.dependency.ratio....of.working.age.population..Male +                                                                  
                      Age.dependency.ratio....of.working.age.population..Prevalence.of.undernourishment....of.population. +                      
                      Death.rate..crude..per.1.000.people..Prevalence.of.undernourishment....of.population. +                                    
                      Life.expectancy.at.birth..total..years..Prevalence.of.undernourishment....of.population. +                                 
                      GDP.growth..annual....Legislation.exists.on.domestic.violence..1.yes..0.no. +                                              
                      Unemployment..total....of.total.labor.force...modeled.ILO.estimate..Subsidies.and.other.transfers....of.expense. +         
                      Adjusted.net.enrollment.rate..primary....of.primary.school.age.children..Final.consumption.expenditure....of.GDP. +        
                      Adjusted.net.enrollment.rate..primary....of.primary.school.age.children..Prevalence.of.undernourishment....of.population. +
                      Expense....of.GDP..Prevalence.of.undernourishment....of.population. +                                                      
                      Military.expenditure....of.general.government.expenditure..Prevalence.of.undernourishment....of.population. +              
                      Annual_precip.Prevalence.of.undernourishment....of.population. +                                                           
                      Both.sexes.Prevalence.of.undernourishment....of.population. +                                                              
                      Female.People.using.at.least.basic.sanitation.services....of.population. +                                                 
                      Female.Prevalence.of.undernourishment....of.population. +
                      Sex.ratio.at.birth..male.births.per.female.births..Prevalence.of.undernourishment....of.population. +                      
                      Legislation.exists.on.domestic.violence..1.yes..0.no..Prevalence.of.undernourishment....of.population. +                   
                      Prevalence.of.undernourishment....of.population..Subsidies.and.other.transfers....of.expense.
                    , data = Double_lasso_data)


p_value_mu = summary(Post_DL_OLS_mu)$coef[,"Pr(>|t|)"]
p_value_mu = p_value_mu[34]

# With a p-value of 0.3478 the interaction effect between undernourishment and the male rate is not causally significant.

#***********************************************
# SUMMARY

p_values = cbind(p_value_li, p_value_eu, p_value_ls, p_value_lu, p_value_mu, p_value_pl, p_value_ur)[,1:7]

# With a level of significance of 98% the life expectance, the interaction between life expectancy and sex ratio, 
# the interaction effect between life expectancy and unemployment as well as the main effect of unemployment are causally significant.