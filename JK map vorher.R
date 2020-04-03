rm(list = ls())

library(tidyverse)
library(tmap)
library(sf)
library(raster)
library(spData)

D = as_tibble(read.csv("../Data/HIV data from world bank.csv"))

D = D %>%
  dplyr::select(1:2,8) %>%
  rename(name_long = names(.)[1]) %>%
  rename(YR2018 = names(.)[3]) %>%
  mutate(YR2018 = replace(YR2018, YR2018 == "..", NA))

D_NA = D[is.na(D$YR2018),]

D0 = D[!is.na(D$YR2018),]

# save(D, D_NA, D0, file = "../Data/HIV.RData")

test = left_join(world, D0)
test$YR2018 =as.integer(test$YR2018)

tmap_mode("view")
tm_shape(test) +
  tm_fill("YR2018", style = "cont", colorNA = NULL)

