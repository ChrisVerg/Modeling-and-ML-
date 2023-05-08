
# Lesson 3 Hands-On

# import dataset nonlinear.csv

# The given dataset has two x and two y variables. Graph each 
# set and determine the relationship of each, exponential or quadratic


colnames(nonlinear)
# "X1" "Y1" "X2" "Y2"


# Variable 1 X1, Y1

# Graph and view the data Variable Set 1
quadPlot <- ggplot(nonlinear, aes(x=X1, y=Y1)) + geom_point() + stat_smooth(
  method = "lm", formula = y~x +I(x^2), linewidth=1)
quadPlot

# This data appears to have an exponential relationship


# Test the quadratic Model
IV_1sq <- nonlinear$X1^2
quadModel1 <- lm(nonlinear$Y1 ~ nonlinear$X1 + IV_1sq)
summary(quadModel1)

# Test the exponential Model
exMod1 <- lm(log(nonlinear$Y1)~nonlinear$X1)
summary(exMod1)





# Variable 1 X2, Y2

# Graph and view the data Variable Set 2
quadPlot <- ggplot(nonlinear, aes(x=X2, y=Y2)) + geom_point() + stat_smooth(
  method = "lm", formula = y~x +I(x^2), linewidth=1)
quadPlot

# This data appears to have an exponential relationship



# Test the quadratic Model
IV_2sq <- nonlinear$X2^2
quadModel2 <- lm(nonlinear$Y2 ~ nonlinear$X2 + IV_2sq)
summary(quadModel2)

# Test the exponential Model
exMod2 <- lm(log(nonlinear$Y2)~nonlinear$X2)
summary(exMod2)


# For both X1Y1 and X2Y2 it appears both the exponential and quadratic model fit the 
# data. The quadratic model is an indication of a constant rate of change and the 
# exponential model is an indication of an increase rate of change. The data appear
# To have just enough curve and just enough "U shape" to be statistically significant 
# for each model. Without underlying information, or context for the data it is difficult
# to select a preferred model for this application.





















