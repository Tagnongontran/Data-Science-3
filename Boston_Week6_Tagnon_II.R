#Name: Tagnon TCHEKLI

# Load the necessary library
library(tidyverse)
library(broom)
library(MASS)

# View the Boston dataset
head(Boston)

# View a description of each variable in the dataset
?Boston

## Part II: Another regression and plot

# Let's conduct the regression of medv on indus and assign it to `mod_bc`
mod_bc <- lm(medv ~ indus, data = Boston)

# Extraction of the estimated model coefficients and assign them to `params`
params <- summary(mod_bc)$coefficients

# Printing of the estimated model coefficients
print(params)
# Estimate Std. Error   t value      Pr(>|t|)
# (Intercept) 29.7548965 0.68344522  43.53662 6.704987e-173
# indus       -0.6484901 0.05226447 -12.40786  4.900260e-31

# Through this model we predict the median value of owner-occupied homes in $1000s (medv) as function of the proportion proportion of non-retail business acres per town.
# The intercept of 29.755 means that the median value of owner-occupied homes in towns where there is no acres for non-retail business is $29,755.
# The slope of -0.648 means that with each additional of an acre for non-retail business per town, the median value of owner occupied homes decreases by $648.
# This model shows a very significant result with a p.value far away below 0.01.

# Let's plot medv against indus
plot(Boston$indus, Boston$medv,
     main = "MEDV vs INDUS",
     xlab = "INDUS (Proportion of non-retail business acres per town)",
     ylab = "MEDV (Median value of owner-occupied homes in $1000's)",
     pch = 19, col = "blue")

# Let's add the regression line to the plot
abline(mod_bc, col = "red")
