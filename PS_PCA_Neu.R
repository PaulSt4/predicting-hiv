library(tidyverse)
#install.packages("randomForest")
library(randomForest)
#install.packages("glmnet")
library(glmnet)
library(ggfortify)

#############PCA
rm(list=ls())
load("./Data/project data.RData")

D_PCA = D %>%
  mutate(Continent = c("Africa", "Europe", "Africa", "Oceania", "Europe", "Africa", "Middle America", "South America", "Europe", "Middle America", "Oceania", "Europe", "Asia", "Middle America", "Asia", "Asia", "Middle America", "Europe", "Europe", "Middle America", "Africa", "Middle America", "Asia", "South America", "Europe", "Africa", "South America","Oceania", "Asia","Europe","Africa","Africa","Africa","Asia","Africa","North America","Middle America","Africa","Africa","Europe","South America","Asia","South America","Africa","Africa","Africa","Middle America","Africa","Europe","Middle America","Middle America","Europe","Europe","Europe","Africa","Middle America","Middle America","South America","Africa","Middle America","Africa","Africa","Europe","Africa","Africa","Europe","Oceania","Europe","Europe","Oceania","Africa","Africa","Africa","Europe","Europe","Africa","Europe","Europe","Middle America","Oceania","Middle America","Africa","Africa","South America","Middle America","Middle America","Asia","Europe","Europe","Asia","Asia","Asia","Asia","Europe","Europe","Asia","Europe","Middle America","Asia","Asia","Asia","Africa","Oceania","Asia","Asia","Europe","Asia","Asia","Asia","Europe","Asia","Africa","Africa","Africa","Europe","Europe","Europe","Asia","Africa","Africa","Asia","Asia","Africa","Europe","Oceania","Africa","Africa","Middle America","Oceania","Europe","Europe","Asia","Europe","Africa","Africa","Asia","Africa","Oceania","Asia","Europe","Oceania","Oceania","Middle America","Africa","Africa","Europe","Oceania","Europe","Asia","Asia","Oceania","Middle America","Asia","South America","South America","Asia","Europe","Europe","Middle America","Asia","Europe","Asia","Africa","Oceania","Europe","Africa","Asia","Africa","Europe","Africa","Africa","Asia","Middle America","Europe","Europe","Oceania","Africa","Africa","Africa","Europe","Asia","Middle America","Middle America","Middle America","Middle America","Africa","South America","Europe","Europe","Asia","Asia","Africa","Asia","Asia","Africa","Oceania","Middle America","Africa","Asia","Asia","Oceania","Oceania","Africa","Europe","Asia","Europe","North America","South America","Asia","Oceania","South America","Asia","Oceania","Asia","Asia","Africa","Africa"))
D_PCA$HIVper100000 = scale(D_PCA$HIVper100000)

D_PCA[, 'Continent'] <- as.factor(D_PCA[, 'Continent'])


hiv.pca <- prcomp(D_PCA[,6:43], center = TRUE,scale. = TRUE)
summary(hiv.pca)

loading.score = hiv.pca$rotation[,1]
score = abs(loading.score)
rank = sort(score, decreasing = TRUE)
PC1 = rank[1:10]

loading.score2 = hiv.pca$rotation[,2]
score2 = abs(loading.score2)
rank2 = sort(score2, decreasing = TRUE)
PC2 = rank2[1:10]

autoplot(hiv.pca, data = D_PCA, colour = 'Continent', frame = TRUE)

# autoplot(hiv.pca, data = D_PCA, colour = 'HIVper100000')
