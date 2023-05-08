
# Stepwise Regression Hands-On



#                               Part 1
#import data IQ.csv

# Based on data of 15 people who each took 5 different IQ tests run backwards
# stepwise regression to determine the best model

# create linear model
Backwards <- lm(IQ ~ ., data = IQ)
summary(Backwards)

# No individual test is a significant predictor

step(Backwards, direction = "backward")

#   Question 1
# The model lm(IQ ~ Test1 + Test2 + Test4, data = IQ) is the best based on 
# backwards regression because it has the lowest AIC score. 

testBackwards<- lm(IQ ~ Test1 + Test2 + Test4, data = IQ)
summary(testBackwards)

#    Question 2 
# The adjusted R squared is 0.2158 meaning that the model predicts the 
# IQ about 22% of the time. 

#    Question 3
# From the best model Test 1 & 2 do no have a pvalue less than .05 so are not
# statistically significant. 
# Test 4 has a pvalue of less than .05 and has the most influence on the IQ.





###############################################################################



##                                 Part 2


# import dataset stepwiseRegression.csv

# There are 12 IVs X1 - X12 that predict the DV labeled as Y.
# Run all three stepwise regressions and answer the questions.

###### Backward Model
Backmodel <- lm(Y ~ ., data = stepwiseRegression)
summary(Backmodel)

step(Backmodel, direction = 'backward')

# The best model based on backward stepwise regression is
# lm(formula = Y ~ X2 + X4 + X6 + X10 + X11 + X12, data = stepwiseRegression)
testBack<- lm(Y ~ X2 + X4 + X6 + X10 + X11 + X12, data = stepwiseRegression)
summary(testBack)

# Based on the backward method 99.9% of the outcome is attributed to the
# selected IV's with AIC of 213.38


###### Forward Model
ForwardModel<- lm(Y ~ 1, data = stepwiseRegression)
summary(ForwardModel)


step(ForwardModel, direction = 'forward', scope = (formula(Backmodel)))

# The best model based on forward method is:
# lm(formula = Y ~ X6 + X4 + X12 + X10 + X2 + X11, data = stepwiseRegression)
# with AIC of 213.38


TestForwardModel <- lm(Y ~ X6 + X4 + X12 + X10 + X2 + X11, data = stepwiseRegression)
summary(TestForwardModel)
# base on this method the R2 is 99.9% of the outcome is based on the IVs in the model


#######     Hybrid Method

step(ForwardModel, direction = "both", scope = formula(Backmodel))

# The best model for this method is:
# lm(formula = Y ~ X6 + X4 + X12 + X10 + X2 + X11, data = stepwiseRegression)
# with AIC of 213.38

testBoth<- lm(Y ~ X6 + X4 + X12 + X10 + X2 + X11, data = stepwiseRegression)
summary(testBoth)

# The r2 shows 99.9% of the outcome is based on the IVs from the model.




# The model that was best for each method was the same, using the exact same IVs,
# X2 X4 X6 X10 X11 X12. Each method suggested the same model with the same AIC
# of 213.38. 
# Any best model of the 3 could be selected, the R squared was the same for each 
# with 99.9% of the outcome being predicted by the model. 








