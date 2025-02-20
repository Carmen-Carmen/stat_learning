library(ISLR2)
attach(Wage)

# 7.8.1 Polynomial Regression and Step Functions
# poly() returns a matrix whose columns are a basis of orthogonal polynomials, 
# which essentially means that each column is a linear combination of 
# the variables age, age^2, age^3 and age^4.
fit = lm(wage ~ poly(age, 4), data = Wage)
coef(summary(fit))
#               Estimate Std. Error t value  Pr(>|t|)
# (Intercept)     111.70     0.7287 153.283 0.000e+00
# poly(age, 4)1   447.07    39.9148  11.201 1.485e-28
# poly(age, 4)2  -478.32    39.9148 -11.983 2.356e-32
# poly(age, 4)3   125.52    39.9148   3.145 1.679e-03
# poly(age, 4)4   -77.91    39.9148  -1.952 5.104e-02

# adding raw = TRUE to obtain age, age^2, age^3 and age^4 directly
fit2 = lm(wage ~ poly(age, 4, raw = T), data = Wage)
coef(summary(fit2))
#                          Estimate Std. Error t value  Pr(>|t|)
# (Intercept)            -1.842e+02  6.004e+01  -3.067 0.0021803
# poly(age, 4, raw = T)1  2.125e+01  5.887e+00   3.609 0.0003124
# poly(age, 4, raw = T)2 -5.639e-01  2.061e-01  -2.736 0.0062606
# poly(age, 4, raw = T)3  6.811e-03  3.066e-03   2.221 0.0263978
# poly(age, 4, raw = T)4 -3.204e-05  1.641e-05  -1.952 0.0510386

# other equivalent ways of fitting the raw model
fit2a = lm(wage ~ age + I(age^2) + I(age^3) + I(age^4), 
           data = Wage)
coef(fit2a)
# (Intercept)         age    I(age^2)    I(age^3)    I(age^4) 
# -1.842e+02   2.125e+01  -5.639e-01   6.811e-03  -3.204e-05

# using cbind() to generate a matrix
fit2b = lm(wage ~ cbind(age, age^2, age^3, age^4), 
           data = Wage)
# -1.842e+02, 2.125e+01, -5.639e-01, 6.811e-03, -3.204e-05

# create a grid of values for age at which we want predictions
agelims = range(age)
age.grid = seq(from = agelims[1], to = agelims[2])
preds = predict(fit, newdata = list(age = age.grid), se = T) # also returns the standard errors of predictions
se.bands = cbind(preds$fit + 2 * preds$se.fit, 
                 preds$fit - 2 * preds$se.fit) # 2 * SE covers 95% CI

par(mfrow = c(1, 2), 
    mar = c(4.5, 4.5, 1, 1), 
    oma = c(0, 0, 4, 0)) # mar and oma arguments control the margins of the plot
# plot the results of degree-4 polynomial fitting using orthogonal polynomials
plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Degree-4 Polynomial", outer = T)
lines(age.grid, preds$fit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3) # plot matrix, since se.bands is a matrix generated by cbind()
# degree-4 polynomial fitting using the raw = TRUE
preds2 = predict(fit2, newdata = list(age = age.grid), se = TRUE)
max(abs(preds$fit - preds2$fit)) # 7.816e-11, indicating little difference in prediction

# decision on the degre of the polynomial to use
# using hypothesis tests to determine the simplest model which is sufficient to explain the relationship between wage and age
fit.1 = lm(wage ~ age, data = Wage)
fit.2 = lm(wage ~ poly(age, 2), data = Wage)
fit.3 = lm(wage ~ poly(age, 3), data = Wage)
fit.4 = lm(wage ~ poly(age, 4), data = Wage)
fit.5 = lm(wage ~ poly(age, 5), data = Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5)
# Analysis of Variance Table
# 
# Model 1: wage ~ age
# Model 2: wage ~ poly(age, 2)
# Model 3: wage ~ poly(age, 3)
# Model 4: wage ~ poly(age, 4)
# Model 5: wage ~ poly(age, 5)
#   Res.Df     RSS Df Sum of Sq      F Pr(>F)    
# 1   2998 5022216                               
# 2   2997 4793430  1    228786 143.59 <2e-16 ***
# 3   2996 4777674  1     15756   9.89 0.0017 ** 
# 4   2995 4771604  1      6070   3.81 0.0510 .  
# 5   2994 4770322  1      1283   0.80 0.3697    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# since poly() creates orthogonal polynomials, checking the results of fit.5
# gets the same output of anova, i.e. F-values from the anova are squared t-values
# from the results below
coef(summary(fit.5))
#               Estimate Std. Error  t value  Pr(>|t|)
# (Intercept)     111.70     0.7288 153.2780 0.000e+00
# poly(age, 5)1   447.07    39.9161  11.2002 1.491e-28
# poly(age, 5)2  -478.32    39.9161 -11.9830 2.368e-32
# poly(age, 5)3   125.52    39.9161   3.1446 1.679e-03
# poly(age, 5)4   -77.91    39.9161  -1.9519 5.105e-02
# poly(age, 5)5   -35.81    39.9161  -0.8972 3.697e-01

# predicting whether an individual earns more than $250,000 per year
fit = glm(I(wage > 250) ~ poly(age, 4), 
          data = Wage, 
          family = binomial)
summary(fit)
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# (Intercept)       -4.301      0.345  -12.46   <2e-16 ***
#   poly(age, 4)1   71.964     26.118    2.76   0.0059 ** 
#   poly(age, 4)2  -85.773     35.904   -2.39   0.0169 *  
#   poly(age, 4)3   34.163     19.689    1.74   0.0827 .  
#   poly(age, 4)4  -47.401     24.091   -1.97   0.0491 * 
preds = predict(fit, newdata = list(age = age.grid), se = T)
preds # the logit, or log-odds were generated, i.e. log(P / (1 - P))
p_fit = exp(preds$fit) / (1 + exp(preds$fit))
p_fit # transformed to the correct probability
se.bands.logit = cbind(preds$fit + 2 * preds$se.fit, 
                       preds$fit - 2 * preds$se.fit)
se.bands = exp(se.bands.logit) / (1 + exp(se.bands.logit))
se.bands # always positive

# another way is to add type = "response" when calling predict()
preds = predict(fit, newdata = list(age = age.grid), 
                type = "response", se = T)
cbind(preds$fit + 2 * preds$se.fit, 
      preds$fit - 2 * preds$se.fit) # not sensible negative probability got

plot(age, I(wage > 250), xlim = agelims, type = "n", ylim = c(0, .2))
points(jitter(age), I((wage > 250) / 5), cex = .5, pch = "|", col = "darkgrey")
lines(age.grid, p_fit, lwd = 2, col = "blue")       
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)

# fitting a step function
table(cut(age, 4))
# (17.9,33.5]   (33.5,49]   (49,64.5] (64.5,80.1] 
#         750        1399         779          72
fit = lm(wage ~ cut(age, 4), data = Wage)
coef(summary(fit))
#                         Estimate Std. Error t value  Pr(>|t|)
# (Intercept)              94.158      1.476  63.790 0.000e+00
# cut(age, 4)(33.5,49]     24.053      1.829  13.148 1.982e-38
# cut(age, 4)(49,64.5]     23.665      2.068  11.443 1.041e-29
# cut(age, 4)(64.5,80.1]    7.641      4.987   1.532 1.256e-01

# 7.8.2 Splines
library(splines)
fit = lm(wage ~ bs(age, knots = c(25, 40, 60)), 
         # bs() generates the entire matrix of basis functions, cubic splines are produced by default
         data = Wage)
pred = predict(fit, newdata = list(age = age.grid), se = T)
par(mfrow = c(1, 1))
plot(age, wage, col = "gray")
lines(age.grid, pred$fit, lwd = 2)
matlines(age.grid, 
  cbind(
    pred$fit + 2 * pred$se.fit, 
    pred$fit - 2 * pred$se.fit
  ), lty = "dashed"
)

dim(bs(age, knots = c(25, 40, 60))) # 3000    6
dim(bs(age, df = 6))
