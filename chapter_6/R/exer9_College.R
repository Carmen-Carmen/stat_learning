library(ISLR2)
library(leaps)
library(glmnet)
library(pls)

# predict the number of applications received 
# using the other variables in the College data set.
names(College)
?College

# (a) Split the data set into a training set and a test set.
set.seed(1)
train = sample(nrow(College), nrow(College) * 2/3)
College.train = College[train, ]
College.test = College[-train, ]

# (b) Fit a linear model using least squares on the 
# training set, and report the test error obtained.
lm.fit = lm(Apps ~ ., data = College.train)
summary(lm.fit)
lm.err = mean((predict(lm.fit, College.test) - College.test$Apps) ^ 2)
lm.err # 1218487

# (c) Fit a ridge regression model on the training set, 
# with λ chosen by cross-validation. Report the test error obtained.
x.test = model.matrix(Apps ~ ., data = College.test)[, -1]
y.test = College.test$Apps
x.train = model.matrix(Apps ~ ., data = College.train)[, -1]
y.train = College.train$Apps

ridge.mod = glmnet(x.train, y.train, alpha = 0)
cv.out = cv.glmnet(x.train, y.train, alpha = 0)
plot(cv.out)
best_lambda = cv.out$lambda.min
best_lambda # 394.2
ridge.pred = predict(ridge.mod, s = best_lambda, newx = x.test)
ridge.err = mean((ridge.pred - y.test) ^ 2)
ridge.err # 1112086

# (d) Fit a lasso model on the training set, with λ chosen by cross-validation. 
# Report the test error obtained, along with the number of non-zero coeﬀicient estimates.
lasso.mod = glmnet(x.train, y.train, alpha = 1)
cv.out = cv.glmnet(x.train, y.train, alpha = 1)
best_lambda = cv.out$lambda.min
best_lambda # 12.68
lasso.pred = predict(lasso.mod, s = best_lambda, newx = x.test)
lasso.err = mean((lasso.pred - y.test) ^ 2)
lasso.err # 1177895

# (e) PCR
pcr.fit = pcr(Apps ~ ., data = College.train, 
              scale = TRUE, 
              validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")
summary(pcr.fit) # 17
pcr.pred = predict(pcr.fit, College.test, ncomp = 17)
pcr.err = mean((pcr.pred - College.test$Apps) ^ 2)
pcr.err # 1218487

# (f) PLS
pls.fit = plsr(Apps ~ ., data = College.train, scale = TRUE, validation = "CV")
validationplot(pls.fit, val.type = "MSEP")
summary(pls.fit) # 15
pls.pred = predict(pls.fit, College.test, ncomp = 15)
pls.err = mean((pls.pred - College.test$Apps) ^ 2)
pls.err # 1218513

# (g)
barplot(c(lm.err, ridge.err, lasso.err, pcr.err, pls.err), 
        names.arg = c("lm", "ridge", "lasso", "PCR", "PLS"),
        ylab = "test MSE")
# the MSE of ridge regression is the lowest