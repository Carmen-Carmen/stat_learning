set.seed(1)
x1 = runif(100)
x2 = 0.5 * x1 + rnorm(100) / 10
y = 2 + 2 * x1 + 0.3 * x2 + rnorm(100)
# the form of the linear model: 
# Y = β0 + β1 * X1 + β2 * X2 + ε

plot(x1, x2, pch = ".", cex = 5)
# the relationship between x1 and x2 appears to be linear

model = lm(y ~ x1 + x2)
summary(model)
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     2.1305     0.2319   9.188 7.61e-15 ***
#   x1            1.4396     0.7212   1.996   0.0487 *  
#   x2            1.0097     1.1337   0.891   0.3754  

# the estimated β0 is close to real intercept
# while the estimated β1 and β2 are both different from the real coefficients
# I can reject the H0 that β1 = 0 since its p-value = 0.0487, 
# though I cannot reject the H0 that β2 = 0 since its p-value = 0.03754.

model2 = lm(y ~ x1)
summary(model2)
# there is a strong linear relationship between x1 and y
# I can easily reject the H0 that β1 = 0 since its p-value is close to zero.

model3 = lm(y ~ x2)
summary(model3)
# similarly, there is also a storng linear relationship between x2 and y

# the results of model2 and model3 do not contradict with that of model
# because x1 and x2 are collinear to some extent, increasing the difficulty of 
# confirming their coefficients in the multiple linear regression.

par(mfrow = c(2, 2))
x1 = c(x1, 0.1)
x2 = c(x2, 0.8)
y = c(y, 6)
model4 = lm(y ~ x1 + x2)
summary(model4)
plot(model4)
# the new observation is a high-leverage point, but not an outlier

model5 = lm(y ~ x1)
summary(model5)
plot(model5)
# the new observation is both a high-leverage point and an outlier

model6 = lm(y ~ x2)
summary(model6)
plot(model6)
# the new observation is an outlier, but not a high-leverage point