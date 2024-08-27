library(ISLR2)
library(leaps)
library(glmnet)
library(pls)

# (a) Use the rnorm() function to generate a predictor X of length n = 100, 
# as well as a noise vector ϵ of length n = 100.
set.seed(1)
x = rnorm(100, 1, 1)
err = rnorm(100, 5, 10)

# (b) Generate a response vector Y of length n = 100 according to the model
# Y = beta_0 + beta_1 * X + beta_2 * X^2 + beta_3 * X^3 + err
y = 1 + 2 * x + 3 * x ^ 2 + 4 * x ^ 3 + err
plot(x, y)

# (c) Use the regsubsets() function to perform best subset selection 
# in order to choose the best model containing the predictors 
# X, X^2, . . . , X^10
# Create a DataFrame with columns X^1 to X^10
df <- data.frame(sapply(1:10, function(i) x^i))
colnames(df) <- paste0("X^", 1:10)

df = cbind(df, Y = y)

regfit.full = regsubsets(Y ~ ., data = df, 
                         nvmax = 10)
summary(regfit.full)
par(mfrow = c(1, 1))
plot(regfit.full, scale = "r2")

plot(regfit.full, scale = "adjr2" ) # X^3 + X^5 + X^6 + X^10
coef(regfit.full, 4)
# (Intercept)       `X^3`       `X^5`       `X^6` 
#   5.7366306  11.9961167  -3.3186443   0.9172749 
# `X^10` 
# -0.0008569 

plot(regfit.full, scale = "Cp" ) # X^3 + X^5 + X^6
coef(regfit.full, 3)
# (Intercept)       `X^3`       `X^5`       `X^6` 
#     6.3679      9.7784     -1.7475      0.3992 

plot(regfit.full, scale = "bic" )# X^1 + X^3
coef(regfit.full, 2)
# (Intercept)       `X^1`       `X^3` 
#       5.829       5.512       4.500

# (d) Repeat (c), using forward stepwise selection 
# and also using backwards stepwise selection.
# forward selection
regfit.fwd = regsubsets(Y ~ ., data = df, 
                        nvmax = 10, method = "forward")
plot(regfit.fwd, scale = "r2")

plot(regfit.fwd, scale = "adjr2" ) # 4
coef(regfit.fwd, 4)
# (Intercept)       `X^1`       `X^2`       `X^3` 
#   4.0287492   5.6484712   3.9790204   2.6874481 
# `X^10` 
# 0.0001745

plot(regfit.fwd, scale = "Cp" ) # 2
coef(regfit.fwd, 2)
# (Intercept)       `X^1`       `X^3` 
#       5.829       5.512       4.500 

plot(regfit.fwd, scale = "bic" ) # 2
coef(regfit.fwd, 2)
# (Intercept)       `X^1`       `X^3` 
#       5.829       5.512       4.500 

# backward selection
regfit.bwd = regsubsets(Y ~ ., data = df, 
                        nvmax = 10, method = "backward")
plot(regfit.bwd, scale = "r2")

plot(regfit.bwd, scale = "adjr2" ) 4
coef(regfit.bwd, 4)
# (Intercept)       `X^3`       `X^5`       `X^6`       `X^8` 
#     5.60769    12.53220    -4.03244     1.29812    -0.02584 

plot(regfit.bwd, scale = "Cp" ) # 3
coef(regfit.bwd, 3)
# (Intercept)       `X^3`       `X^5`       `X^6` 
#     6.3679      9.7784     -1.7475      0.3992 

plot(regfit.bwd, scale = "bic" )
coef(regfit.bwd, 3)
# (Intercept)       `X^3`       `X^5`       `X^6` 
#     6.3679      9.7784     -1.7475      0.3992 

# (e) fit a lasso model to the simulated data, again
# Use cross-validation to select the optimal value of λ. 
# Create plots of the cross-validation error as a function of λ.
x = model.matrix(Y ~ ., data = df)[, -1] 
y = df$Y
grid = 10^seq(10, -2, length = 100)
lasso.mod = glmnet(x, y, alpha = 1, lambda = grid)
plot(lasso.mod)
cv.out = cv.glmnet(x, y, alpha = 1, lambda = grid)
plot(cv.out)
best_lambda = cv.out$lambda.min # 0.6753
lasso.coef = predict(lasso.mod, type = "coefficients", s = best_lambda)[1:11, ]
lasso.coef
# (Intercept)       `X^1`       `X^2`       `X^3`       `X^4`       `X^5`       `X^6` 
#   5.053e+00   5.021e+00   3.312e+00   2.983e+00   0.000e+00   0.000e+00   0.000e+00 
#     `X^7`       `X^8`       `X^9`      `X^10` 
# 0.000e+00   0.000e+00   2.825e-04   4.816e-05 

# (f) generate a response vector Y according to the model
# Y = beta_0 + beta_7 * X^7 + err
y = 100 + x ^ 7 + err
df$Y = y
plot(x, y)
# best subset selection
regfit.full = regsubsets(Y ~ ., data = df, 
                         nvmax = 10)
plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2" ) # 8
coef(regfit.full, 8)
# (Intercept)       `X^1`       `X^3`       `X^5`       `X^6`       `X^7`       `X^8` 
#     4.8275     -1.1834     25.9702    -27.0621     12.0852      7.3950     -7.2220 
# `X^9`      `X^10` 
# 2.0245     -0.1937
plot(regfit.full, scale = "Cp" ) # 3
coef(regfit.full, 3)
# (Intercept)       `X^3`       `X^5`       `X^6` 
#     6.3679      9.7784     -1.7475      0.3992 
plot(regfit.full, scale = "bic" )
coef(regfit.full, 1)
# (Intercept)       `X^3` 
#       9.582       5.094

# lasso
set.seed(1)
x = model.matrix(Y ~ ., data = df)[, -1] 
y = df$Y
cv.out = cv.glmnet(x, y, alpha = 1, lambda = grid)
plot(cv.out)
best_lambda = cv.out$lambda.min # 0.0933
lasso.out = glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef = predict(lasso.out, type = "coefficients", 
                     s = best_lambda)[1:11, ]
lasso.coef
# (Intercept)       `X^1`       `X^2`       `X^3`       `X^4`       `X^5`       `X^6` 
#   1.047e+02   0.000e+00   0.000e+00   0.000e+00   0.000e+00   0.000e+00   8.327e-02 
#     `X^7`       `X^8`       `X^9`      `X^10` 
# 9.603e-01   3.157e-03   2.074e-04   2.409e-05