# This  is a script for the
# regression chtapter

# load packages
library(tidyverse)

# load data
parenthood <- read.csv("/Users/hhadah/Documents/GiT/teaching-demo/parenthood.csv")

# Check the lm function
?lm

# check the names of the vars
names(parenthood)

# Run regression 
regression <- lm(formula = grumpiness ~ hours_slept, data = parenthood)

# results
print(regression)
summary(regression)
tidy(regression) # broom package
library(broom)
library(modelsummary)
modelsummary(regression) # modelsummary package

# predict grumpiness at 5 hours of sleep

## by hand:
125.9563 + (-8.9368 * 5)

## Using predict() function
grumpiness <- data.frame(hours_slept = 5)
?predict
predict(regression, newdata = grumpiness)

# multiple linear regression
regression2 <- lm(formula = grumpiness ~ hours_slept + dogs_sleep, data = parenthood)
summary(regression2)

# calculate Y hat
X <- parenthood$hours_slept # predictor
Y <- parenthood$grumpiness # outcome

Y.pred <- -8.94 * X + 125.97

# calculate sum squared residuals (SSR):
SS.res <- sum((Y - Y.pred)^2)
SS.res
# calculate total sum of squares (SST):
SS.tot <- sum((Y - mean(Y))^2)
SS.tot

# calculate R squared
R.squared <- 1 - (SS.res / SS.tot)
print( R.squared )

# correlation
r <- cor(X, Y)
print(r^2)

