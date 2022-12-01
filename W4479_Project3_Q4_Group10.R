library(lmtest)
library(tidyverse)

#######################################################################################################
######################################################## a) ###########################################
#######################################################################################################

# reading of dataset "Housing Prices"

house.prices = read.csv("Housing Prices.csv")

# regression 1

reg1 = lm(Price ~ LotSize + Age + Rooms, data = house.prices)
summary(reg1)

# 95% confidence intervals

confint(reg1, parm = "LotSize")

confint(reg1, parm = "Age")

confint(reg1,  parm = "Rooms")

# correlations 

# with LotSize
cor(house.prices$Price, house.prices$LotSize)
cor(house.prices$LotSize, house.prices$Age)
cor(house.prices$LotSize, house.prices$Rooms)


# with Age
cor(house.prices$Price, house.prices$Age)
cor(house.prices$Age, house.prices$LotSize)
cor(house.prices$Age, house.prices$Rooms)


# with Rooms
cor(house.prices$Price, house.prices$Rooms)
cor(house.prices$Rooms,house.prices$LotSize)
cor(house.prices$Rooms, house.prices$Age)

#######################################################################################################
######################################################## b) ###########################################
#######################################################################################################

# durbin Watson test - Calculation of d-statistics

residual_1 = reg1$residuals
order_livingarea = order(house.prices$LivingArea)
u = residual_1[order_livingarea]
n = length(u)

d = sum((u[2:n] - u[1:(n-1)])^2)/sum(u^2)
d

dwtest(reg1, order.by = house.prices$LivingArea)

########################################################################################################
######################################################## c) ############################################
########################################################################################################

# regression 2

reg2 = lm(Price ~ LotSize + Age + Rooms + LivingArea, data = house.prices)
summary(reg2)

confint(reg2, parm = "LotSize")

confint(reg2, parm = "Age")

confint(reg2,  parm = "Rooms")

########################################################################################################
####################################################### d) #############################################
########################################################################################################

residual_2 = reg2$residuals

# NewConstruct

order_nc = order(house.prices$NewConstruct)
u = residual_2[order_nc]
n = length(u)

d = sum((u[2:n] - u[1:(n-1)])^2)/sum(u^2)
d

dwtest(reg2, order.by = house.prices$NewConstruct)


# Bedrooms

order_b = order(house.prices$Bedrooms)
u = residual_2[order_b]
n = length(u)

d = sum((u[2:n] - u[1:(n-1)])^2)/sum(u^2)
d

dwtest(reg2, order.by = house.prices$Bedrooms)

# LandValue

order_lv = order(house.prices$LandValue)
u = residual_2[order_lv]
n = length(u)

d = sum((u[2:n] - u[1:(n-1)])^2)/sum(u^2)
d

dwtest(reg2, order.by = house.prices$LandValue)


# CentralAir

order_ca = order(house.prices$CentralAir)
u = residual_2[order_ca]
n = length(u)

d = sum((u[2:n] - u[1:(n-1)])^2)/sum(u^2)
d

dwtest(reg2, order.by = house.prices$CentralAir)

#######################################################################################################
##################################################### e) ##############################################
#######################################################################################################

reg3 = lm(Price ~ LotSize + Age + Rooms + LivingArea + LandValue, data = house.prices)
summary(reg3)

aic = c(AIC(reg1), AIC(reg2), AIC(reg3))
bic = c(BIC(reg1), BIC(reg2), BIC(reg3))

aic
bic
