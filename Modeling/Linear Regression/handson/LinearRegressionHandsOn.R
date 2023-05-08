# Lesson 1 Hands on
# import packages

library("car")
library("caret")
library("gvlma")
library("predictmeans")
library("e1071")
library("lmtest")

# Import Dataset heights
# computer linear regression
# IV heights$AM_Height
# DV heights$PM_Height


#####    Test Assumptions

## Test for linearity between X and Y
scatter.smooth(x=heights$AM_Height, y=heights$PM_Height, main= "Morning and Evening heights")
# assumption of linearity is met

## Test for Homoscedasticity 
# create model 
heightsmodel <- lm(PM_Height~AM_Height, data= heights)

# plot the model
par(mfrow=c(2,2))
plot(heightsmodel)
# based on the plot, the line is not straight and the assumption is not met

# Breusch Pagan Test
lmtest::bptest(heightsmodel)
# pvalue is not significant

# NCV test
car::ncvTest(heightsmodel)
# pvalue is not significant



## Test for Homogeneity of variance
gvlma(heightsmodel)
# all assumptions met based on gvlma function



## Screening for Outliers

# Leverage using CooksD
CookD(heightsmodel, group = NULL, plot = TRUE, idn = 3, newwd = TRUE)
# 3,4 and 12 noted



lev = hat(model.matrix(heightsmodel))
plot(lev)

heights[lev>.2,]
# One outlier noted above .2


# Distance outliers / y space
car::outlierTest(heightsmodel)
# pvalue is significant


# Testing for outliers in X and Y space

summary(influence.measures(heightsmodel))
# Nothing greater than 1 in DFITS or DFBETAS


### Assess Regression Model results
summary(heightsmodel)
# Significant F-statistic and p-value indicates there is probably an
# effect of the IV on the DV in this case, the R squared indicates that
# 99% of the variance can be linked to the IV. 



