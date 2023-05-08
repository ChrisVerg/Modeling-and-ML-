

# Non-Linear Modeling   y = ax2 + bx + c

# Import packages

library("ggplot2")

# import data bluegill_fish.csv

# The data set has age and length of bluegill fish. The question is,
# does the age of the fish influence their length

fish <- bluegill_fish

# Graph the quadratic equation using ggplot

quadPlot <- ggplot(fish, aes(x=age, y=length)) + geom_point() + stat_smooth(
  method = "lm", formula = y~x +I(x^2), linewidth=1)
quadPlot

##### Model the Quadratic Relationship

# square X in this model
agesq <- fish$age^2


quadModel <- lm(fish$length ~ fish$age + agesq)
summary(quadModel)


######   Exponential Modeling 
# base R can do this, no packages needed

# import dataset bacteria.csv

# How much does bacteria grow over time?

# Log of Y for this model
exMod <- lm(log(bacteria$Count)~bacteria$Period)
summary(exMod)
# significant p value and f statistic indicates the growth
# is exponential. 








