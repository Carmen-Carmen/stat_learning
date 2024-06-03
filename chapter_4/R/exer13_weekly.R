library(ISLR2)
attach(Weekly)
?Weekly
summary(Weekly)
names(Weekly)

# (a)
cor(Weekly[, 1:8])
plot(Weekly)
# volume increases as year increases

# (b)
log_reg = glm(Direction ~ Volume + Lag1 + Lag2 + Lag3 + Lag4 + Lag5, 
              data = Weekly, family = binomial)
summary(log_reg)
#   Coefficients:
#               Estimate Std. Error z value Pr(>|z|)   
#   (Intercept)  0.26686    0.08593   3.106   0.0019 **
#   Volume      -0.02274    0.03690  -0.616   0.5377   
#   Lag1        -0.04127    0.02641  -1.563   0.1181   
#   Lag2         0.05844    0.02686   2.175   0.0296 * 
#   Lag3        -0.01606    0.02666  -0.602   0.5469   
#   Lag4        -0.02779    0.02646  -1.050   0.2937   
#   Lag5        -0.01447    0.02638  -0.549   0.5833
# Lag2 appears to be statistically significant.

# (c)
log_reg.probs = predict(log_reg, type = "response")
contrasts(Direction)
log_reg.pred = rep("Down", nrow(Weekly))
log_reg.pred[log_reg.probs > .5] = "Up"
table(log_reg.pred, Direction)
#             Direction
# log_reg.pred Down  Up
#         Down   54  48
#         Up    430 557
# type I mistake: 430 / (430 + 54) = 88.9%
# type II mistake: 48 / (48 + 557) = 7.9%
mean(log_reg.pred == Direction) # 56.1% overall accuracy

# (d)
summary(Year)
train_filter = (Year <= 2008) # filter
Weekly.test = Weekly[!train_filter,]
log_reg.train = glm(Direction ~ Lag2, data = Weekly, 
                    family = binomial, subset = train_filter)
log_reg.train.probs = predict(log_reg.train, Weekly.test, type = "response")
log_reg.train.pred = rep("Down", nrow(Weekly.test))
log_reg.train.pred[log_reg.train.probs > .5] = "Up"
Direction.test = Direction[!train_filter]
table(log_reg.train.pred, Direction.test)
#                 Direction.test
# log_reg.train.pred Down Up
#               Down    9  5
#               Up     34 56
mean(log_reg.train.pred == Direction.test) # 62.5%

# (e) repeat using LDA
library(MASS)
lda.model = lda(Direction ~ Lag2, data = Weekly, subset = train_filter)
lda.pred = predict(lda.model, Weekly.test) # type = "raw" can be omitted
table(lda.pred$class, Direction.test)
#     Direction.test
#       Down Up
# Down    9  5
# Up     34 56
names(lda.pred)
mean(lda.pred$class == Direction.test) # 62.5%

# (f) repeat using QDA
qda.model = qda(Direction ~ Lag2, data = Weekly, subset = train_filter)
qda.pred = predict(qda.model, Weekly.test, type = "raw")
table(qda.pred$class, Direction.test)
# Direction.test
#       Down Up
# Down    0  0
# Up     43 61
mean(qda.pred$class == Direction.test) # 58.6%

# (g) repeat using KNN with K = 1
set.seed(1)
library(class)
# train.X = matrix(Lag2[train_filter])
train.X = Weekly[train_filter, "Lag2", drop = FALSE]
# test.X = matrix(Lag2[!train_filter])
test.X = Weekly[!train_filter, "Lag2", drop = FALSE]
train.Y = Direction[train_filter]

knn.pred = knn(train.X, test.X, train.Y, k = 1)
tab = table(knn.pred, Direction.test)
tab
#     Direction.test
# knn.pred Down Up
#     Down   21 29
#     Up     22 32
sum(diag(tab)) / sum(tab) # 51.0%

# (h) repeat using naive Bayes
library(e1071)
nb.model = naiveBayes(Direction ~ Lag2, data = Weekly, 
                      subset = train_filter)
nb.pred = predict(nb.model, Weekly.test)
tab = table(nb.pred, Direction.test)
tab
#     Direction.test
# nb.pred Down Up
#   Down    0  0
#   Up     43 61
sum(diag(tab)) / sum(tab) # 58.6%

# (i) LDA and logistic regression provided the best results

# (j)
accuracy_list = c()
for (k_val in 1:40) {
  knn.pred = knn(train.X, test.X, train.Y, k = k_val)
  tab = table(knn.pred, Direction.test)
  accuracy = sum(diag(tab)) / sum(tab)
  s = sprintf("overall accuracy for k = %d: %.2f%%", k_val, accuracy * 100)
  print(s)
  accuracy_list = c(accuracy_list, accuracy)
}
plot(1:40, accuracy_list, type = "o", 
     xlab = "k", ylab = "overall accuracy")
