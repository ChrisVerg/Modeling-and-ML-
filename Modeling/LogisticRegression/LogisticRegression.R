# Import Packages

library("caret")
library("magrittr")
library("dplyr")
library("tidyr")
library("lmtest")
library("popbio")
library("e1071")


# import data baseball.csv

# Run a regression with the predictor is the number of home runs hit by a team 
# and the response variable is whether the team wins or loses. 

# Recode wins and losses to binary

baseball$WinLossR <- NA
baseball$WinLossR[baseball$`W/L`== "W"] <- 1
baseball$WinLossR[baseball$`W/L`== "L"] <- 0

baseball$WinLossR <- as.numeric(baseball$WinLossR)


# Create Base Logistic Model

model <- glm(WinLossR ~ `HR Count`, data = baseball, family = "binomial")

# use predict to predict Wins and Losses

probablilty <- predict(model, type = "response")
# this creates probablities for for winning based on HR

# Using .5 threshold use ifelse to split the probabilities into likely or unlikely
baseball$predicted <- ifelse(probablilty > .5, "pos", "neg")

# Recode predicted in to binary
baseball$predictedR <- NA
baseball$predictedR[baseball$predicted== "pos"] <- 1
baseball$predictedR[baseball$predicted== "neg"] <- 0

# convert to factor

baseball$predictedR <- as.factor(baseball$predictedR)
baseball$WinLossR <- as.factor(baseball$WinLossR)


####   Test Assumptions

## Sample Size
# Create confusion matrix
conf_matrix <- caret::confusionMatrix(baseball$predictedR, baseball$WinLossR)
conf_matrix



## Logit Linearity

baseball1 <- baseball %>% dplyr::select_if(is.numeric)

predictors <- colnames(baseball1)

baseball1 <- baseball1 %>%
  mutate(logit=log(probablilty/(1-probablilty))) %>%
  gather(key= "predictors", value="predictor.value", -logit)

ggplot(baseball1, aes(logit, predictor.value))+
  geom_point(size=.5, alpha=.5)+
  geom_smooth(method= "loess")+
  theme_bw()+
  facet_wrap(~predictors, scales="free_y")
# The graph with HR shows a strong linear relationship

## Multicollinearity
# This is to determine relatedness of IVs. In this calculation there is only 
# one IV so this assumption is N/A

## Independent Errors
# Graph the residual over the index and us Durbin-Watson Test
plot(model$residuals)

dwtest(model, alternative="two.sided")
# pvalue
# For the DW statistic if it is less than 1 or more than 3 you have violated the
# assumption


# Screening for Outliers
outlier <- influence.measures(model)
summary(outlier)
# This is a lot to manually scan




###### Running logistic Regression and interpreting the output
summary(model)

logi.hist.plot(baseball$`HR Count` ,baseball$WinLossR , boxp=FALSE, type="hist", col="gray")

