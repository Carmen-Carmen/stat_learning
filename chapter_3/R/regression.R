library(MASS)
library(ISLR2)

head(Boston)
?Boston

# simple linear regression
fit1 = lm(medv ~ lstat, data = Boston)
attach(Boston)
fit1 = lm(medv ~ lstat)
fit1
summary(fit1)
names(fit1)
coef(fit1)

# obtain a confidence interval for the coefficient estimates
confint(fit1)

# predict() function used to produce confidence intervals and prediction intervals
# for the prediction of medv for a given value fo lstat
predict(fit1, data.frame(lstat = (c(5, 10, 15))), interval = "confidence")
predict(fit1, data.frame(lstat = (c(5, 10, 15))), interval = "prediction")

plot(lstat, medv)
abline(fit1) # plot a line by the slope and interception

abline(fit1, lwd = 3) # increase the width of regression line by 3
abline(fit1, lwd = 3, col = "red")

plot(lstat, medv, col = "red")
plot(lstat, medv, pch = 20) # point characters
plot(lstat, medv, pch = "+")
plot(x = seq(1, 20), y = seq(1, 20), pch = 1:20, cex = 3)

# results of the linear model
par(mfrow = c(2, 2))
plot(fit1)

# can also plot the above graph using residuals() and rstudent(), i.e. studentized residuals
plot(predict(fit1), residuals(fit1))
plot(predict(fit1), rstudent(fit1))
plot(hatvalues(fit1))
which.max(hatvalues(fit1)) # identifies the index of the largest element of a vector, telling us which observations has the largest leverage statistic
par(mfrow = c(1, 1))

# multiple linear regression
# specify 2 predictors
fit2 = lm(medv ~ lstat + age, data = Boston)
summary(fit2)

# specify all the predictors
fit3 = lm(medv ~ ., data = Boston)
summary(fit3)
?summary.lm
summary(fit3)$r.sq # r-squared
summary(fit3)$sigma # RSE

library(car) # Companion to Applied Regression
vif(fit3) # variance inflation factors, a measurement of multicolliniarity

# since age has a high p-val, we run a refgression without this predictor
fit4 = lm(medv ~ . - age, data = Boston) # "~ . - age" means to fit against all predictors except "age"
summary(fit4)
fit4 = update(fit3, ~ . - age) # update a previous model with new predictors
summary(fit4)

# interaction terms
summary(lm(medv ~ lstat * age, data = Boston))
# R will automatically add each predictor from the product
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 36.0885359  1.4698355  24.553  < 2e-16 ***
#   lstat       -1.3921168  0.1674555  -8.313 8.78e-16 ***
#   age         -0.0007209  0.0198792  -0.036   0.9711    
# lstat:age    0.0041560  0.0018518   2.244   0.0252 * 
# lm(medv ~ I(lstat * age)) means only considering the interaction, i.e. fitting against the product of istat and age

# non-linear transformation of the predictors
fit5 = lm(medv ~ lstat + I(lstat ** 2))
summary(fit5)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 42.862007   0.872084   49.15   <2e-16 ***
#   lstat       -2.332821   0.123803  -18.84   <2e-16 ***
#   I(lstat^2)   0.043547   0.003745   11.63   <2e-16 *** 
# the near-zero p-value with the quadratic term suggests that it leads to an improved model
# use anova() to further quantify the extent to which the quadratic fit is superior to the linear fit
anova(fit1, fit5)
# Model 1: medv ~ lstat
# Model 2: medv ~ lstat + I(lstat^2)
#   Res.Df   RSS Df Sum of Sq     F    Pr(>F)    
# 1    504 19472                                 
# 2    503 15347  1    4125.1 135.2 < 2.2e-16 ***
# anova() performs a hypothesis test comparing the 2 models
# H0: the 2 models fit the data equally well
# Ha: the full model is superior

par(mfrow = c(2, 2))
plot(fit5)
par(mfrow = c(1, 1))

# poly() orthogonalized the predictors, meaning the features output by the function is 
# not simply a sequence of powers of the argument
# though for linear model, the output will have the same fitted values as lm(medv ~ lstat + I(lstat ^ 2)) + ... + I(lstat ^ 5)
fit6 = lm(medv ~ poly(lstat, 5))
summary(fit6)

# to obtain the raw polynominals from the poly() function, the argument raw = True must be added
fit6 = lm(medv ~ poly(lstat, 5), raw = True) # though extra argument "raw" will be discarded in linear models
summary(fit6)

# also can fit against log(predictor)
fit7 = lm(medv ~ log(lstat))
summary(fit7)
par(mfrow = c(2, 2))
plot(fit7)
par(mfrow = c(1, 1))

# Qualitative predictors
?Carseats
head(Carseats)
names(Carseats)
fit8 = lm(Sales ~ . + Income:Advertising + Price:Age, data = Carseats)
summary(fit8)
# ShelveLocGood and ShelveLocMedium are dummy variables automatically generated by R
# while ShelveLocBad is the baseline
contrasts(Carseats$ShelveLoc) # returns the coding that R uses for the dummy variables
#         Good Medium
# Bad       0      0
# Good      1      0
# Medium    0      1

# self-written functions
LoadLibraries = function() {
  libraries = c("MASS", "ISLR2")
  for (lib in libraries) {
    library(lib, character.only = T)
  }
  to_print = sprintf("Libraries [%s] has been loaded.", 
                     paste(libraries, collapse = ", "))
  print(to_print)
}

LoadLibraries()
