set.seed(1)

x = rnorm(100)
eps = rnorm(100, 0, .25)
y = -1 + 0.5 * x + eps

length(y)
# the length of vector y is 100
# beta_0 = -1, beta_1 = 0.5

plot(x, y, cex = 5, pch = ".")
# the relationship between x and y appears to be linear

model = lm(y ~ x)
summary(model)
# the estimated beta_0 and beta_1 are close to the real ones.

abline(model, lwd = 2, col = "red")
abline(-1, 0.5, lwd = 2, col = "blue", lty = 2)
legend("bottomright", legend = c("model fit", "population regression"), 
       col = c("red", "blue"), 
       lty = c(1, 2))

# fit a polynomial regression that predicts y using x and x^2
model_quatratic = lm(y ~ x + I(x ^ 2))
summary(model_quatratic)
anova(model, model_quatratic)
# no significant improvement according to the non-significant F-test

# less noise
eps = rnorm(100, 0, .01)
y2 = -1 + 0.5 * x + eps
model2 = lm(y2 ~ x)
summary(model2)
plot(x, y2, pch = ".", cex = 5)
abline(model2, lwd = 2, col = "red")
abline(-1, 0.5, lwd = 2, col = "blue", lty = 2)
legend("bottomright", legend = c("model fit", "population regression"), 
       col = c("red", "blue"), 
       lty = c(1, 2))
# the estimated beta_0 and beta_1 are closer to -1 and 0.5

# more noise
eps = rnorm(100, 0, .5)
y3 = -1 + 0.5 * x + eps
model3 = lm(y3 ~ x)
summary(model3)
plot(x, y3, pch = ".", cex = 5)
abline(model3, lwd = 2, col = "red")
abline(-1, 0.5, lwd = 2, col = "blue", lty = 2)
legend("bottomright", legend = c("model fit", "population regression"), 
       col = c("red", "blue"), 
       lty = c(1, 2))
# the estimated beta_0 and beta_1 are farther away from -1 and 0.5
# and the model fit line diverges more with the population regression line
