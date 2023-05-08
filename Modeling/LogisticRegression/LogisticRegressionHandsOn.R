# Logistic Regression Hands On

# Import Packages
library("caret")
library("magrittr")
library("dplyr")
library("tidyr")
library("lmtest")
library("popbio")
library("e1071")

# Import data minerals.csv

# This data shows where antimony(Sb) is found for 64 locations and if gold was
# found nearby. Use antimony as the IV and determine if it is a good predictor of 
# gold being found. 
# 0 represents no Gold found
# 1 represents Gold found

# The predicted value is already recoded to binary

# Create base model
model <- glm(Gold~Antimony, data= minerals, family = "binomial")

# Determine probability
probability <- predict(model, type = "response")

# Using .5 threshold create a column that represent Antimony in binary format
minerals$predicted <- ifelse(probability > .5, "pos", "neg")

# Recode
# Recode predicted in to binary
minerals$predictedR <- NA
minerals$predictedR[minerals$predicted== "pos"] <- 1
minerals$predictedR[minerals$predicted== "neg"] <- 0

# Convert both variables to 
minerals$predictedR <- as.factor(minerals$predictedR)
minerals$Gold <- as.factor(minerals$Gold)

# Create confusion matrix
conf_matrix <- caret::confusionMatrix(minerals$predictedR, minerals$Gold)
conf_matrix

###### Assumptions 


## Sample size not met
# For the sample of when gold was predicted but not found is only 2. 


### Logit Linearity
minerals1 <- minerals %>% dplyr::select_if(is.numeric)

predictors <- colnames(minerals1)

minerals2 <- minerals1 %>%
  mutate(logit=log(probability/(1-probability))) %>%
  gather(key= "predictors", value="predictor.value", -logit)

ggplot(minerals2, aes(logit, predictor.value))+
  geom_point(size=.5, alpha=.5)+
  geom_smooth(method= "loess")+
  theme_bw()+
  facet_wrap(~predictors, scales="free_y")

## Multicollinearity
# This is to determine relatedness of IVs. In this calculation there is only 
# one IV so this assumption is N/A

## Independent Errors
# Graph the residual over the index and us Durbin-Watson Test
plot(model$residuals)

#DWtest
dwtest(model, alternative="two.sided")
?dwtest


## Screeing for outliers
outlier <- influence.measures(model)
summary(outlier)



## Run the regression
summary(model)

# It appears that logistic regression confirms the belief that antimony can be 
# used to predict the presence of gold near by. 







