rm(list = ls())

library(tidyverse)
library(mice)

# Read Data and tidy up ----

D = as_tibble(read.csv("../Data/worldbank data.csv", na.strings = ".."))
D = rename(D,Series.Name = names(D)[1])

names(D)
D = select(D, -X2015..YR2015., -X2016..YR2016., -X2018..YR2018., -X2019..YR2019., -Series.Code)
D = rename(D, X2017 = names(D)[4])

D = pivot_wider(D[1:10850,], names_from = "Series.Name", values_from = "X2017")

names(D)
D = select(D, Country.Name, Country.Code, "Adults (ages 15+) newly infected with HIV", "Population, total", everything())
D[62,4] = 5068831
D = mutate(D, HIVper100000 = (D$`Adults (ages 15+) newly infected with HIV` / D$`Population, total`)*100000)

D = select(D, names(D)[1:3], HIVper100000, everything())

sapply(D, class)
summary(D)

# add climate data

temp = as_tibble(read.csv("../Data/avg. yearly temp.csv", sep = ";"))
temp = rename(temp,Country.Code = names(temp)[1])
temp = temp[,c(1,14)]

precip = as_tibble(read.csv("../Data/avg. yearly preci.csv", sep = ";"))
precip = rename(precip,Country.Code = names(precip)[1])
precip = precip[,c(1,14)]

D = left_join(D,temp)
D = left_join(D, precip)

rm(temp, precip)

# add BMI

bmi = as_tibble(read.csv("../Data/BMI (men-women-overall).csv", skip = 3, na.strings = "No data"))
bmi = rename(bmi,Country.Name = names(bmi)[1])
bmi$Both.sexes = as.numeric(gsub("\\[.*","", bmi$Both.sexes))
bmi$Male = as.numeric(gsub("\\[.*","", bmi$Male))
bmi$Female = as.numeric(gsub("\\[.*","", bmi$Female))
D = left_join(D, bmi)
rm(bmi)

# add doctors

doc = as_tibble(read.csv("../Data/doctors.csv")) %>%
  filter(Year == 2016) %>%
  select(1, 3) %>%
  rename(Country.Name = names(.)[1])
D = left_join(D,doc)
rm(doc)

# add married women

women = as_tibble(read.csv("../Data/married women.csv")) %>%
  rename(Country.Name = names(.)[1])
women = women[-1,]
D = left_join(D, women)
rm(women)

# add sex ratio
sexr = as_tibble(read.csv("../Data/sex ratio.csv")) %>%
  rename(Country.Name = names(.)[1]) %>%
  rename("Sex ratio at birth (male births per female births)" = names(.)[5]) %>%
  select(-1, -3, -4)
D = left_join(D, sexr)
rm(sexr)

# add worldbank general indicators
wbgeneral = as_tibble(read.csv("../Data/World bank general indicators.csv", na.strings = "..")) %>%
  rename(Country.Name = names(.)[1]) %>%
  select(-(4:6)) %>%
  slice(-(5426:nrow(.))) %>%
  pivot_wider(names_from = Series.Name, values_from = X2017..YR2017.)
D = left_join(D,wbgeneral)
rm(wbgeneral)

# add worldbank health indicators
wbhealth = as_tibble(read.csv("../Data/World bank health indicators.csv", na.strings = "..")) %>%
  select(-(2)) %>%
  slice(-(2171:nrow(.))) %>%
  rename(Series.Name = names(.)[1]) %>%
  pivot_wider(names_from = Series.Name, values_from = X2017..YR2017.)
D = left_join(D,wbhealth)
rm(wbhealth)

# drop columns where more than half of the observations are missing
dropped = names(D[, colSums(is.na(D)) >= 103.5])
D = D[, colSums(is.na(D)) < 103.5]

# imputate missing values using mice

n = 1:length(names(D))
for (i in n) {
  # i = 1
  newnames = make.names(names(D))[i]
  D = rename(D, !!newnames := names(D)[i])
}
rm(i,n,newnames)

imp = mice(D[,-c(1:5)], m = 1)
imp = complete(imp)
imp_s = sapply(imp, as.numeric)
imp_s = scale(imp_s)

D = cbind(D[,1:5], imp_s)
D = select(D, -Unemployment..total....of.total.labor.force., -Population..male....of.total.population.)
rm(imp,imp_s)

# split data set

D_NA = D[is.na(D$Adults..ages.15...newly.infected.with.HIV),]
legend_NA = D_NA[c(1,2,3,5)]
D_NA = D_NA[-c(1,2,3,5)]

D0 = D[!is.na(D$Adults..ages.15...newly.infected.with.HIV),]
legend0 = D0[c(1,2,3,5)]
D0 = D0[-c(1,2,3,5)]


# save(list = ls(), file = "../Data/project data.RData")
