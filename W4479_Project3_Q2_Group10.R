library(tidyverse)
library(AER)
library(car)
library(carData)
###Fitting regression model###
data("CASchools")
CAS= data.frame(CASchools [, c(4,5,6,7,8,9,10,11,12,13,14)])
reg_all= lm(income~., data = CAS)
summary(reg_all)
###Calculate VIF and TOL on all independent variables###

VIF= vif(reg_all)
TOL= 1/VIF
VIF
TOL
###Fitting further regression model after removing multicollinearity###
income_wo_students= select(CAS, -students)
income_wo_teachers= select(CAS, -teachers)
income_wo_read= select(CAS, -read)
income_wo_multicollinearity= select(CAS, -students, -teachers, -read)
reg_wo_students= lm(income~., data = income_wo_students)
reg_wo_teachers= lm(income~., data = income_wo_teachers)
reg_wo_read= lm(income~., data=income_wo_read)
reg_wo_multicollinearity= lm(income~., data = income_wo_multicollinearity)
vif(reg_wo_students)
vif(reg_wo_teachers)
vif(reg_wo_read)
vif(reg_wo_multicollinearity)
reg_all
reg_wo_students
reg_wo_teachers
reg_wo_read
reg_wo_multicollinearity
###Calculating AIC & BIC###
Aic_all= AIC(reg_all)
Aic_wo_students= AIC(reg_wo_students)
Aic_wo_teachers= AIC(reg_wo_teachers)
Aic_wo_read= AIC(reg_wo_read)
Aic_wo_multicollinearity= AIC(reg_wo_multicollinearity)
Aic_all;Aic_wo_students;Aic_wo_teachers;Aic_wo_read;Aic_wo_multicollinearity

Bic_all= BIC(reg_all)
Bic_wo_students= BIC(reg_wo_students)
Bic_wo_teachers= BIC(reg_wo_teachers)
Bic_wo_read= BIC(reg_wo_read)
Bic_wo_multicollinearity= BIC(reg_wo_multicollinearity)
Bic_all;Bic_wo_students;Bic_wo_teachers;Bic_wo_read;Bic_wo_multicollinearity

