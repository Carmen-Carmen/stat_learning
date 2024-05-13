library(ISLR2)

?Boston
significant_predictors = c()
for (p in names(Boston)) {
  if (p != "crim") {
    form = as.formula(paste("crim ~", p))
    model = lm(form, data = Boston)
    summary_model = summary(model)
    p_value = summary_model$coefficients[p, "Pr(>|t|)"]
    if (p_value < 0.05) {
      significant_predictors = c(significant_predictors, p)
    }
  }
}

par(
  mfrow = c(
      length(significant_predictors) %/% 4, 4
    )
  )
for (p in significant_predictors) {
  plot(Boston[[p]], Boston[["crim"]], 
       xlab = p, ylab = "crim")
}
par(mfrow = c(1, 1))

# pred = subset(Boston, select = -crim)
# fits = lapply(pred, function(x) lm(Boston$crim ~ x))
# printCoefmat(do.call(rbind, lapply(fits, function(x) coef(summary(x))[2, ])))

# multiple linear regression
model = lm(crim ~ ., data = Boston)
summary(model)
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  17.033228   7.234903   2.354 0.018949 *  
# zn            0.044855   0.018734   2.394 0.017025 *  
# indus        -0.063855   0.083407  -0.766 0.444294    
# chas         -0.749134   1.180147  -0.635 0.525867    
# nox         -10.313535   5.275536  -1.955 0.051152 .  
# rm            0.430131   0.612830   0.702 0.483089    
# age           0.001452   0.017925   0.081 0.935488    
# dis          -0.987176   0.281817  -3.503 0.000502 ***
# rad           0.588209   0.088049   6.680 6.46e-11 ***
# tax          -0.003780   0.005156  -0.733 0.463793    
# ptratio      -0.271081   0.186450  -1.454 0.146611    
# black        -0.007538   0.003673  -2.052 0.040702 *  
# lstat         0.126211   0.075725   1.667 0.096208 .  
# medv         -0.198887   0.060516  -3.287 0.001087 ** 
# H0:βj = 0 can be rejected for: zn, dis, rad, black, medv

univariate_coefficients = c()
for (p in names(Boston)) {
  if (p != "crim") {
    form = as.formula(paste("crim ~", p))
    model = lm(form, data = Boston)
    summary_model = summary(model)
    univariate_coefficients = c(univariate_coefficients, summary_model$coefficients[2])
  }
}
multiple_regression_coefficients = c()
model = lm(crim ~ ., data = Boston)
for (cef in model$coefficients) {
  multiple_regression_coefficients = c(multiple_regression_coefficients, cef)
}
multiple_regression_coefficients = multiple_regression_coefficients[-1]
plot(univariate_coefficients, multiple_regression_coefficients, 
     xlab = "univariate coefficients", 
     ylab = "multiple regression coefficients")

# non-linear associations
for (p in names(Boston)) {
  if (p != "crim") {
    form_str = sprintf("crim ~ %s + I(%s^2) + I(%s^3)", p, p, p)
    form = as.formula(form_str)
    model = lm(form, data = Boston)
    printCoefmat(coef(summary(model)))
  }
}
# non-linear relationships in predictors: indus, age, dis, ptratio, medv
par(mfrow = c(2, 3))
for (p in c("indus", "age", "dis", "ptratio", "medv")) {
  plot(Boston[[p]], Boston[["crim"]], 
       xlab = p, ylab = "crim")
}
