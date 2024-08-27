library(ISLR2)
library(leaps)
library(glmnet)
library(pls)

# (a) Generate a data set with p = 20 features, n = 1,000 observations, and 
# an associated quantitative response vector generated according to the model
# Y = beta * X + err
set.seed(11)
df = data.frame(1:1000)
for (i in 1:20) {
  x = rnorm(1000, i * 10, 50)
  df = cbind(df, x)
}
df = df[, -1]
summary(df)

betas = runif(20, 0, 50)
zeros = sample(length(betas), length(betas) * 1/2)
betas[zeros] = 0
betas

y = as.matrix(df) %*% betas
df$Y = y

# (b) Split your data set into a training set containing 100 observations 
# and a test set containing 900 observations.
train = sample(1000, 100)
df.test = df[-train, ]
df.train = df[train, ]

# (c) Perform best subset selection on the training set, 
# and plot the training set MSE associated with the best model of each size.
regfit.full = regsubsets(Y ~ ., data = df.train, nvmax = 20)
summary(regfit.full)
train.matrix = model.matrix(Y ~ ., data = df.train)
regfit.train_errors = rep(NA, 20)
for (i in 1:20) {
  coefi = coef(regfit.full, id = i)
  pred = train.matrix[, names(coefi)] %*% coefi
  regfit.train_errors[i] = mean((pred - df.train$Y) ^ 2)
}
regfit.train_errors
min_train_error_index = which.min(regfit.train_errors)
plot(1:20, 
     regfit.train_errors, 
     xlab = "number of predictors", 
     ylab = "training error")
points(min_train_error_index, min(regfit.train_errors), col = "green")

# (d) Plot the test set MSE associated with the best model of each size.
test.matrix = model.matrix(Y ~ ., data = df.test)
regfit.test_errors = rep(NA, 20)
for (i in 1:20) {
  coefi = coef(regfit.full, id = i)
  pred = test.matrix[, names(coefi)] %*% coefi
  regfit.test_errors[i] = mean((pred - df.test$Y) ^ 2)
}
min_test_error_index = which.min(regfit.test_errors)
plot(1:20, 
     regfit.test_errors, 
     xlab = "number of predictors", 
     ylab = "test error")
points(min_test_error_index, min(regfit.test_errors), col = "green")

# (e) For which model size does the test set MSE take on its minimum value?
# 10-size model has the min test MSE
sum(betas != 0) # 10

# (f) How does the model at which the test set MSE is minimized 
# compare to the true model used to generate the data?
best_coefs = coef(regfit.full, 10)
best_coefs
betas = as.data.frame(t(betas))
colnames(betas) = names(df)[-20]
betas
diff_beta_est = betas
for (colname in names(betas)) {
  if (!is.na(best_coefs[colname])) {
    diff_beta_est[colname] = betas[colname] - best_coefs[colname]
  }
}
diff_beta_est
# nearly no difference

# (g)
diff_beta_est = 1:20
for (i in 1:20) {
  coefs = coef(regfit.full, i)
  temp = 0
  for (colname in names(betas)) {
    if (!is.na(coefs[colname])) {
      temp = temp + (betas[colname] - coefs[colname])[1] ^ 2
    }
  }
  temp = sqrt(temp)
  diff_beta_est[[i]] = temp[1, 1]
}
diff_beta_est
plot(1:20, 
     diff_beta_est, 
     xlab = "number of predictors", 
     ylab = "DIFF")
points(which.min(diff_beta_est), 
       min(diff_beta_est), 
       col = "green") # 11

model = lm(Y ~ ., df)
summary(model)
