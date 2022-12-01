library(tidyverse)
library(AER)
library(car)
library(datasets)
library(Ecdat)
library(ISLR)

###########################################################################################
############################################# part a) #####################################
###########################################################################################

# dataset CPSSWEducation

data(CPSSWEducation)
?CPSSWEducation

# selection of two variables to build simple linear regression model

data_educ_income = select(CPSSWEducation, earnings, education)
data_educ_income

reg_CPSSWEducation = lm(earnings ~ education, data = data_educ_income)
summary(reg_CPSSWEducation)

# scatterplot to check visibility of heteroscedasticity

plot1 = ggplot(data = data_educ_income, aes(x = education, y = earnings)) + geom_point()
plot1

###########################################################################################
############################################# part b) #####################################
###########################################################################################

# regression model

reg_1 = lm(earnings ~ education, data = data_educ_income)
summary(reg_1)


# scatterplot with regression line

plot2 = ggplot(data = data_educ_income, aes(x = education, y = earnings)) + geom_point() + geom_smooth(method = lm, se = FALSE)
plot2

# residuals 

reg_1_residuals = (reg_1$residuals)
reg_1_residuals


# 95% confidence interval

confint(reg_1, parm = "education")

###########################################################################################
############################################ part c) ######################################
###########################################################################################

# park test

reg_park = lm(I(log(reg_1_residuals^2))~I(log(data_educ_income$education)))
summary(reg_park)

###########################################################################################
############################################ part d) ######################################
###########################################################################################

# OLS estimators

reg_1 = lm(earnings ~ education, data = data_educ_income)
summary(reg_1)

# WLS estimators with weights 1/X

reg_2 = lm(earnings ~ education, weights = I(1/education), data = data_educ_income)
summary(reg_2)

# WLS estimators with weights 1/X^2

reg_3 = lm(earnings ~ education, weights = I(1/education^2), data = data_educ_income)
summary(reg_3)

# 95% confidence intervals for the three regressions

confint(reg_1, parm = "education")

confint(reg_2, parm = "education")

confint(reg_3, parm = "education")


