library(AER)
library(ggplot2)
library(tidyverse)
library(car)
library(ggpubr)
library(tidyr)
library(moments)

#first glance on the data set
data("CPSSW9204")

#a) Filter data for a specific year  

CPSSW9204$year = as.factor(CPSSW9204$year)

cpss_year = filter(CPSSW9204, year == "1992")
head(cpss_year)

#b)Run a regression of earnings on age and degree(dummy variable) in the levels.

cpss_year$degree = ifelse(cpss_year$degree == "highschool", 1, 0)
cpss_year

reg_1 = lm(earnings ~ age + degree, data = cpss_year)
reg_1$coef

summary(reg_1)

#c) Run a regression of earnings on age and degree(dummy variable) in the slopes

reg_2 = lm(earnings ~ age + degree + I(age*degree), data = cpss_year)
reg_2$coef

summary(reg_2)

#d) Run a regression of earnings on age, degree(dummy variable), and gender(dummy variable), and all interaction terms between degree and gender

cpss_year$gender = ifelse(cpss_year$gender == "female", 1, 0)
cpss_year

reg_3 = lm(earnings ~ age + degree + gender + I(age*degree) + I(age*gender) + I(gender*degree), data = cpss_year)
reg_3$coef

summary(reg_3)

