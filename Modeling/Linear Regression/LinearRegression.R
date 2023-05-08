# Linear Regression

# Import packages
library("car")
library("caret")
library("gvlma")
library("predictmeans")
library("e1071")
library("lmtest")

# Using the manatee/Powerboat dataset
# Determine the affect on manatee deaths by the number of power boats registered. 
# No data wrangling needed.

##### Test Assumptions

## Test for Linearity between X an Y
# use scatter.smooth() give the x and y variables and title main =

scatter.smooth(x=manatees$PowerBoats, y=manatees$ManateeDeaths, main= "Manatee deaths from Power Boats")
# this scatter plot shows a linear relationship

## Test for Homoscedasticity
# Normal distribution of the error for X
# create linear model lm(DV~IV, data = ) use + to add IVs
lmMod <- lm(ManateeDeaths~PowerBoats, data = manatees)

# run test
# set up to run 2X2 graphs
par(mfrow=c(2,2))
plot(lmMod)
# This returns 4 graphs to interpret 
# Looking for lines to go straight across on the left
# graphs. In this case the assumption is not met.

# To test with more objective output like pvalue:
# Non-constant Variance Breush-Pagan Test
lmtest::bptest(lmMod)
# which returns pvalue .004 so significant and does not
# meet assumption

# Also:
# Car NCV test
car::ncvTest(lmMod)
# pvalue returned


###    Correcting for Homoscedasticity Violations
# Correcting homoscedasticity violations
# using caret library
# The below code transforms the dependent variable.
distBCMod1 <- caret::BoxCoxTrans(manatees$ManateeDeaths)
print(distBCMod1)


# Now use cbind to add a new variable to the manatees dataset
# the new variable dist_newM will be the predicted data
# from the Box-Cox transformation. 
manatees <- cbind(manatees, dist_newM=predict(distBCMod1, manatees$ManateeDeaths))


# Recreate the model using the new variable as the DV
lmMod_bc2 <- lm(dist_newM~PowerBoats, data=manatees)
lmtest::bptest(lmMod_bc2)

# This example data still fails to meet the assumption
# most cases the above methods will correct for the failure
# Changing regression method is possible but more advanced
# Than this lesson. 


#####       Homogeneity of Variance using (GVLMA)
# Looking at the graphs produced from line 31 in the Code above
# can assess some aspects of Homogeneity of Variance

# gvlma is great but does not test all assumptions for regression
# and may not always be accurate. 
gvlma(lmMod_bc2)

# output explanation  https://github.com/ChrisVerg/DS-Student-Resources/blob/main/DS106-Machine-Learning/Modeling/DS106-L1-Modeling-with-linear-regression.ipynb




####    Screening for Outliers 

# Screen for outliers in x space (leverage) using Cooks Distance Values
# Run the original model in function CookD from predictmeans librart
CookD(lmMod, group = NULL, plot = TRUE, idn = 3, newwd = TRUE)
# newwd = True create graph in new window. Can't use in Jupyter

# To test for leverage 
lev = hat(model.matrix(lmMod))
plot(lev)
# B/w .2 and .5 is problematic
# No issue with this model

# To check for any outliers greater than .2
manatees[lev>.2,]
# Return of 0 indicates there are none. 


# Screen Outliers in Y space (distance) using studentized residual
# is similar to calculating standard error except usng n-1
car::outlierTest(lmMod)
# check bonferonni pvalue and rstudent value over 2.5- 3 
# indicates a problematic outlier



# Screen for outliers in x and y space (influential) using 
# metrics DFITS and DFBETAS

summary(influence.measures(lmMod))
# If DFITS or DFBETAS is greater than 1 there is likely a problem


#####     Running Simple Linear Regression ###

# Now running simple linear regression and interpreting the output
# y = b1x + b0
# y=DV
# X=IV
# b1= reg coefficient/slope
# b0= constant/intercept
# epsilon = error term

# Hypothesis testing 
# hypothesis H0: B1 = 0   there is no linear relation
# hypothesis Ha: B1 = !0 there is a linear relation

# use the model 
summary(lmMod_bc2)

# Significant pvalue and F statistic indicates there is 
# probably an effect of the IV on the DV

# For interpretation use above link 

# Multiple R square is how well the regression line fits
# .87 means 87% of the deaths can be explained by number of 
# power boats. 

# Adjusted R Squared: usually for more than one independent variable

#####    Non-Transformed Regression output
summary(lmMod)











