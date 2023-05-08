
# Lesson 4 Step wise regression

install.packages("mtcars")


head(mtcars)

# determine the mpg using the other 10 IVs in mtcars

# Create a linear model with all 10 predictors

FitAll = lm(mpg~., data = mtcars)

summary(FitAll)
# No individual predictor is significant


##########          Backward stepwise regression


step(FitAll, direction= 'backward')

# AIC stands for Akaike Information Criteria which is the AIC score used 
# to compare models to one another. 

# Determine quality of the model
fitsome = lm(mpg~am + qsec + wt, data = mtcars)
summary(fitsome)



############      Forward Stepwise regression

# Start off with an 'empty' model 1 instead of . tells the model to start empty
# this is called an intercept model
fitstart = lm(mpg~1, data = mtcars)

summary(fitstart)
# When running forward step() use the model for backwards step that includes all
# predictors so you dont have to type them all out manually. 

step(fitstart, direction = 'forward', scope = (formula(FitAll))) 

# Determine the quality of the model
test <- lm(mpg ~ wt + cyl + hp, data = mtcars)
summary(test)




###########    Hybrid Stepwise regression
# Need the models from both forward and backward to do this regression

step(fitstart, direction = "both", scope = formula(FitAll))

















