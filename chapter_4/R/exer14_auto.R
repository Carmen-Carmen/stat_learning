setwd("~/Documents/stat_learning/chapter_4/R")
Auto = read.csv("~/Documents/stat_learning/csv_files/Auto.csv", 
                na.strings = "?", 
                stringsAsFactors = TRUE)
Auto = na.omit(Auto)

mpg01 = rep(0, nrow(Auto))
mpg01[Auto$mpg >= median(Auto$mpg)] = 1
mpg01

Auto$mpg01 = mpg01
Auto
summary(Auto)

# (b)
par(mfrow = c(3, 3))
for (p in names(Auto)) {
  if (p != "mpg01" && p != "mpg") {
    plot(as.factor(mpg01), Auto[[p]], 
           xlab = "mpg01", ylab = p)
  }
}
par(mfrow = c(1, 1))
# features except for "names" seem to be useful in predicting mpg01

plot(Auto[, !(names(Auto) %in% c("name", "mpg01", "mpg"))])
# horsepower, weight and displacement seem to be collinear

# (c) split the Auto data set into train set and test set
dim(Auto)
summary(Auto$year)
attach(Auto)
train_filter = (year < 80)
Auto.train = Auto[train_filter, ]
Auto.test = Auto[!train_filter, ]

# use cylinders, weight and acceleration to predict mpg01

# (d) lda
library(MASS)
lda.model = lda(mpg01 ~ cylinders + weight + acceleration, data = Auto, subset = train_filter)
lda.pred = predict(lda.model, Auto.test)
lda.table = table(lda.pred$class, Auto.test$mpg01)
lda.table
sum(diag(lda.table)) / sum(lda.table) # 87% accuracy
(sum(lda.table) - sum(diag(lda.table))) / sum(lda.table) # 13% error rate

# (e) qda
qda.model = qda(mpg01 ~ cylinders + weight + acceleration, 
                data = Auto, subset = train_filter)
qda.pred = predict(qda.model, Auto.test)
qda.table = table(qda.pred$class, Auto.test$mpg01)
(sum(qda.table) - sum(diag(qda.table))) / sum(qda.table) # 15% error rate

# (f) logistic regression
log_reg.model = glm(mpg01 ~ cylinders + weight + acceleration, 
                    data = Auto, subset = train_filter, 
                    family = binomial)
log_reg.probs = predict(log_reg.model, Auto.test, type = "response")
log_reg.pred = rep(0, nrow(Auto.test))
log_reg.pred[log_reg.probs > .5] = 1
log_reg.table = table(log_reg.pred, Auto.test$mpg01)
(sum(log_reg.table) - sum(diag(log_reg.table))) / sum(log_reg.table) # 20% error rate
mean(log_reg.pred != Auto.test$mpg01)

# (g) naive Bayes
library(e1071)
nb.model = naiveBayes(mpg01 ~ cylinders + weight + acceleration, 
                      data = Auto, subset = train_filter)
nb.pred = predict(nb.model, Auto.test) # type = "class", i.e. 0 or 1
nb.table = table(nb.pred, Auto.test$mpg01)
(sum(nb.table) - sum(diag(nb.table))) / sum(nb.table) # 9.4%

# (h) KNN
library(class)
train.X = Auto[train_filter, c("cylinders", "weight", "acceleration"), drop = FALSE]
test.X = Auto[!train_filter, c("cylinders", "weight", "acceleration"), drop = FALSE]
train.Y = Auto[train_filter, "mpg01"]
set.seed(1)
error_list = c()
for (k_val in 1:40) {
  knn.pred = knn(train.X, test.X, train.Y, k = k_val)
  knn.table = table(knn.pred, Auto.test$mpg01)
  error_rate = (sum(knn.table) - sum(diag(knn.table))) / sum(knn.table)
  error_list = c(error_list, error_rate)
}
plot(1:40, error_list, type = "o", 
     xlab = "k", ylab = "error_rate")
error_list[25] # lowest error rate for KNN is 16.5% for k = 25 and 26
