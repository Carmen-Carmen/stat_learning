library(ISLR2)
library(leaps)
library(glmnet)
library(pls)

# try to predict per capita crime rate in the Boston data set.
?Boston

# (a) Try out some of the regression methods explored in this chapter, 
# such as best subset selection, the lasso, ridge regression, and PCR

# best subset selection
k = 10
n = nrow(Boston)
set.seed(1)
folds = sample(rep(1:k, length = n))
cv.errors = matrix(NA, k, 12, dimnames = list(NULL, paste(1:12)))
predict.regsubsets = function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  xvars = names(coefi)
  pred = mat[, xvars] %*% coefi
  return(pred)
}
for (j in 1:k) {
  regfit.full = regsubsets(crim ~ ., 
                           data = Boston[folds != j, ], 
                           nvmax = 12)
  
  for (i in 1:12) {
    pred = predict(regfit.full, Boston[folds == j, ], id = i)
    cv.errors[j, i] = mean((Boston$crim[folds == j] - pred) ^ 2)
  }
}
mean.cv.errors = apply(cv.errors, 2, mean)
which.min(mean.cv.errors) # 11
regfit.err = min(mean.cv.errors)
regfit.err # 42.68

x = model.matrix(crim ~ ., Boston)[, -1]
y = Boston$crim
# ridge regression
ridge.cv.out = cv.glmnet(x, y, alpha = 0)
plot(ridge.cv.out)
best_lambda = ridge.cv.out$lambda.min
ridge.err = min(ridge.cv.out$cvm)
ridge.err # 43.17

# lasso regression
lasso.cv.out = cv.glmnet(x, y, alpha = 1)
plot(lasso.cv.out)
lasso.err = min(lasso.cv.out$cvm)
lasso.err # 42.77

# PCR
pcr.fit = pcr(crim ~ ., data = Boston, scale = TRUE, validation = "CV")
cv_errors = pcr.fit$validation$PRESS
which.min(cv_errors) # 12
pcr.err = min(cv_errors / nrow(Boston))
pcr.err # 42.36

# validationplot(pcr.fit)
# mse_list = numeric(k)
# for (i in 1:k) {
#   pcr.train = pcr(crim ~ ., data = Boston[folds == i, ], ncomp = 12)
#   pcr.pred = predict(pcr.train, Boston[folds != i, ], ncomp = 12)
#   
#   mse = mean((pcr.pred - Boston$crim[folds != i]) ^ 2)
#   mse_list[i] = mse
# }
# pcr.err = mean(mse_list)
# pcr.err # 58.05

# PLS
pls.fit = plsr(crim ~ ., data = Boston, scale = TRUE, validation = "CV")
cv_errors = pls.fit$validation$PRESS
which.min(cv_errors) # 9
pls.err = min(cv_errors / nrow(Boston))
pls.err # 42.22

# validationplot(pls.fit)
# mse_list = 1:k
# for (i in 1:k) {
#   pls.train = plsr(crim ~ ., data = Boston[folds == i, ], ncomp = 12)
#   pls.pred = predict(pls.train, Boston[folds != i, ], ncomp = 12)
#   
#   mse = mean((pls.pred - Boston$crim[folds != i]) ^ 2)
#   mse_list[i] = mse
# }
# pls.err = mean(mse_list)
# pls.err # 58.05

barplot(c(regfit.err, ridge.err, lasso.err, pcr.err, pls.err), 
        names.arg = c("subset", "ridge", "lasso", "PCR", "PLS"),
        ylab = "test MSE")

# (c)
# PLS is the best
pls.fit = plsr(crim ~ ., data = Boston, ncomp = 9)
summary(pls.fit)
