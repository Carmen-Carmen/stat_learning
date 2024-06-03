library(ISLR2)

setwd("~/Documents/stat_learning/chapter_4/R")

names(Smarket)
dim(Smarket)

# goal: predict Direction using other features.
# Lag1 ... Lag5: the percentage returns for each of the 5 previous trading days
# Volume: the number of shares traded on the previous day
# Today: the percentage return on the date in question
# Direction: whether the markes was Up or Down

summary(Smarket)
cor(Smarket)
# Error in cor(Smarket): "x" must be numeric
# remove the qualitative Direction variable
cor(Smarket[, -9])

attach(Smarket)
plot(Volume)

# logistic regression
# family = binomial indicates performing a logistic regression
glm.fits = glm(
  Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
  data = Smarket, family = binomial
)
summary(glm.fits)
coef(glm.fits)
# access coefficients
summary(glm.fits)$coef
# access particular coefficients, e.g. the P-values
summary(glm.fits)$coef[, "Pr(>|z|)"] # summary(glm.fits)$[, 4]

# if no data set is supplied, then the probabilities are computed for the training data
glm.probs = predict(glm.fits, type = "response")
glm.probs[1:40]
contrasts(Direction)
glm.pred = rep("Down", nrow(Smarket))
glm.pred[glm.probs > .5] = "Up"
table(glm.pred, Direction)
#         Direction
# glm.pred Down  Up
#     Down  145 141
#     Up    457 507
sum(glm.pred == Direction) / nrow(Smarket)
mean(glm.pred == Direction)
(145 + 507) / 1250
# training error rate = 47.8%, too high

# more realistic scenario, using data 2001 ~ 2004 to predict Direction in 2005
training_set = (Year < 2005) # a vector of 1250 elements, with True when Year < 2005, and False when Year = 2005
Smarket.2005 = Smarket[!training_set, ] # reverse False to True, then use the reversed vector to fetch data in 2005
dim(Smarket.2005)
Direction.2005 = Direction[!training_set]
glm.fits_training = glm(
  Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
  data = Smarket, family = binomial, subset = training_set
)
summary(glm.fits_training)
glm.probs_testing = predict(glm.fits_training, Smarket.2005, type = "response")
glm.pred_testing = rep("Down", nrow(Smarket.2005))
glm.pred_testing[glm.probs_testing > .5] = "Up"
table(glm.pred_testing, Direction.2005)
#                 Direction.2005
# glm.pred_testing Down Up
#             Down   77 97
#             Up     34 44
mean(glm.pred_testing == Direction.2005) # 48.0%
mean(glm.pred_testing != Direction.2005) # 52.0%

# remove non-significant predictors
# Coefficients:
#              Estimate Std. Error z value Pr(>|z|)
# (Intercept)  0.191213   0.333690   0.573    0.567
# Lag1        -0.054178   0.051785  -1.046    0.295
# Lag2        -0.045805   0.051797  -0.884    0.377
# Lag3         0.007200   0.051644   0.139    0.889
# Lag4         0.006441   0.051706   0.125    0.901
# Lag5        -0.004223   0.051138  -0.083    0.934
# Volume      -0.116257   0.239618  -0.485    0.628
glm.fits_new = glm(
  Direction ~ Lag1 + Lag2, data = Smarket, 
  family = binomial, subset = training_set
)
glm.probs_new = predict(glm.fits_new, Smarket.2005, type = "response")
glm.pred_new = rep("Down", nrow(Smarket.2005))
glm.pred_new[glm.probs_new > .5] = "Up"
table(glm.pred_new, Direction.2005)
#             Direction.2005
# glm.pred_new Down  Up
#         Down   35  35
#         Up     76 106
mean(glm.pred_new == Direction.2005) # 56% overall correct rate
106 / (76 + 106) # 58% correct when predicting Up

predict(
  glm.fits_new, 
  newdata = data.frame(Lag1 = c(1.2, 1.5), Lag2 = c(1.1, 0.8)), 
  type = "response"
)

# Linear Discriminant Analysis
library(MASS)
lda.fit = lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = training_set)
# Prior probabilities of groups:
#   Down       Up 
# 0.491984 0.508016 
# 
# Group means:
#             Lag1        Lag2
# Down  0.04279022  0.03389409
# Up   -0.03954635 -0.03132544
# 
# Coefficients of linear discriminants:
#             LD1
# Lag1 -0.6420190
# Lag2 -0.5135293
lda.fit
plot(lda.fit)

lda.pred = predict(lda.fit, Smarket.2005)
names(lda.pred)
# [1] "class"     "posterior" "x"
table(lda.pred$class, Direction.2005)
# Direction.2005
#       Down  Up
# Down   35  35
# Up     76 106
mean(lda.pred$class == Direction.2005) # 56%
sum(lda.pred$posterior[, 1] >= .5) # 70 days in 2005 has pr > 0.5 to decrease
sum(lda.pred$posterior[, 1] < .5) # 182
lda.pred$posterior[1:20, 1]
lda.pred$class[1:20]
sum(lda.pred$posterior[, "Down"] > .9) # 0 day in 2005 meet the threshold of 0.9

# Quadratic Discriminant Analysis
# qda() is part of the MASS library.
qda.fit = qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = training_set)
qda.fit
# Prior probabilities of groups:
#   Down       Up 
# 0.491984 0.508016 
# 
# Group means:
#             Lag1        Lag2
# Down  0.04279022  0.03389409
# Up   -0.03954635 -0.03132544
qda.class = predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
#     Direction.2005
# qda.class Down  Up
#     Down   30  20
#     Up     81 121
mean(qda.class == Direction.2005) # 60%

# Naive Bayes
library(e1071)
nb.fit = naiveBayes(Direction ~ Lag1 + Lag2, data = Smarket, subset = training_set)
nb.fit
# A-priori probabilities:
#   Y
#     Down       Up 
# 0.491984 0.508016 
# 
# Conditional probabilities:
#   Lag1
# Y             [,1]     [,2]
#   Down  0.04279022 1.227446
#   Up   -0.03954635 1.231668
# 
#   Lag2
# Y             [,1]     [,2]
#   Down  0.03389409 1.239191
#   Up   -0.03132544 1.220765
mean(Lag1[training_set][Direction[training_set] == "Down"]) # 0.0428
sd(Lag1[training_set][Direction[training_set] == "Down"]) # 1.23
nb.class = predict(nb.fit, Smarket.2005)
table(nb.class, Direction.2005)
# Direction.2005
# nb.class Down  Up
#     Down   28  20
#     Up     83 121
mean(nb.class == Direction.2005) # 59.1%, worse than QDA, better than LDA
nb.preds = predict(nb.fit, Smarket.2005, type = "raw") # "raw" returns the probabilities for each class
nb.preds[1:5, ]

# K-Nearest Neighbors
# different from previous parametric model-fitting methods
library(class)
# 1. prepare training data and test data
train.X = cbind(Lag1, Lag2)[training_set, ]
test.X = cbind(Lag1, Lag2)[!training_set, ]
train.Direction = Direction[training_set]

# 2. call knn() with specified k value
set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k = 3)
table(knn.pred, Direction.2005)
#         Direction.2005
# knn.pred Down Up
#     Down   43 58
#     Up     68 83
mean(knn.pred == Direction.2005) # 53.6%, worse than QDA

# caravan dataset
dim(Caravan) # 85 predictors for predicting whether or not a given individual purchases a caravan insurance policy
attach(Caravan)
summary(Purchase)
sum(Purchase == rep("Yes", length(Purchase))) / length(Purchase) # 0.0598
# standardize the data to eliminate problems caused by data scales
standardized.X = scale(Caravan[, -86])
var(Caravan[, 1]) # 165.0
var(Caravan[, 2]) # 0.1647
# all variables are given a mean of 0 and a sd of 1
var(standardized.X[, 1]) # 1
var(standardized.X[, 2]) # 1

test = 1:1000
train.X = standardized.X[-test, ]
test.X = standardized.X[test, ]
train.Y = Purchase[-test]
test.Y = Purchase[test]
set.seed(1)
knn.pred = knn(train.X, test.X, train.Y, k = 1)
mean(test.Y != knn.pred) # 11.8% total error rate
mean(test.Y != "No") # 0.059, though only 6% customers purchased, i.e. the error rate of always predicting "No" is 6% < 11.8%
table(knn.pred, test.Y)
# test.Y
# knn.pred  No Yes
#       No  873  50
#       Yes  68   9
9 / (68 + 9) # 11.7% customers who are predicted to buy actually bought
knn.pred = knn(train.X, test.X, train.Y, k = 3) # with k = 3
table(knn.pred, test.Y)
# test.Y
# knn.pred  No Yes
#       No  920  54
#       Yes  21   5
5 / (21 + 5) # 19.2% for k = 3
knn.pred = knn(train.X, test.X, train.Y, k = 5) # with k = 3
table(knn.pred, test.Y)
# test.Y
# knn.pred  No Yes
#       No  930  55
#       Yes  11   4
4 / (11 + 4) # 26.7% for k = 5

# using logistic regression to fit caravan data
glm.fits = glm(Purchase ~ ., 
               data = Caravan, 
               family = binomial, 
               subset = -test)
glm.probs = predict(glm.fits, Caravan[test, ], type = "response")
glm.pred = rep("No", 1000)
glm.pred[glm.probs > .5] = "Yes"
table(glm.pred, test.Y)
# test.Y
# glm.pred  No Yes
#       No  934  59
#       Yes   7   0
# if use logistic regression with threshold >= 0.5, the result is worse
glm.pred[glm.probs > .25] = "Yes"
table(glm.pred, test.Y)
# test.Y
# glm.pred  No Yes
#       No  919  48
#       Yes  22  11
11 / (22 + 11) # 33.3% customers predicted to buy actually bought.

# Poisson Regression
attach(Bikeshare)
dim(Bikeshare)
names(Bikeshare)

# first, try fitting with linear regression
mod.lm = lm(
  bikers ~ mnth + hr + workingday + temp + weathersit, 
  data = Bikeshare
)
summary(mod.lm)
#   mnthFeb                      6.845      4.287   1.597 0.110398    
#   mnthMarch                   16.551      4.301   3.848 0.000120 ***
#   mnthApril                   41.425      4.972   8.331  < 2e-16 ***
#   mnthMay                     72.557      5.641  12.862  < 2e-16 ***
#   mnthJune                    67.819      6.544  10.364  < 2e-16 ***
#   mnthJuly                    45.324      7.081   6.401 1.63e-10 ***
#   mnthAug                     53.243      6.640   8.019 1.21e-15 ***
#   mnthSept                    66.678      5.925  11.254  < 2e-16 ***
#   mnthOct                     75.834      4.950  15.319  < 2e-16 ***
#   mnthNov                     60.310      4.610  13.083  < 2e-16 ***
#   mnthDec                     46.458      4.271  10.878  < 2e-16 ***

# make the coefficients for the last levels of hr and mnth not zero, i.e. not baseline
# sum contrasts: 
# Each level of the categorical variable is assigned a contrast vector. 
# For a factor with k levels, sum contrasts result in  k − 1 dummy variables. 
# The k-th level is not assigned a dummy variable directly but is inferred 
# such that the sum of all coefficients (including the  k k-th level) equals zero.
contrasts(Bikeshare$hr) = contr.sum(24)
contrasts(Bikeshare$mnth) = contr.sum(12)
mod.lm2 = lm(
  bikers ~ mnth + hr + workingday + temp + weathersit, 
  data = Bikeshare
)
summary(mod.lm2)
#   mnth1                      -46.0871     4.0855 -11.281  < 2e-16 ***
#   mnth2                      -39.2419     3.5391 -11.088  < 2e-16 ***
#   mnth3                      -29.5357     3.1552  -9.361  < 2e-16 ***
#   mnth4                       -4.6622     2.7406  -1.701  0.08895 .  
#   mnth5                       26.4700     2.8508   9.285  < 2e-16 ***
#   mnth6                       21.7317     3.4651   6.272 3.75e-10 ***
#   mnth7                       -0.7626     3.9084  -0.195  0.84530    
#   mnth8                        7.1560     3.5347   2.024  0.04295 *  
#   mnth9                       20.5912     3.0456   6.761 1.46e-11 ***
#   mnth10                      29.7472     2.6995  11.019  < 2e-16 ***
#   mnth11                      14.2229     2.8604   4.972 6.74e-07 ***
# the coefficients for the last levels of hr and mnth will be the negative of the sum of the 
# coefficients estimates for all of the other levels, i.e. the sum of coefficients for all levels will equal 0
# another way of standardization?

# to see whether the coding of qualitative predictors matters or not
sum((predict(mod.lm) - predict(mod.lm2))^2) # nearly 0, though difference in coding of qualitative predictors
all.equal(predict(mod.lm), predict(mod.lm2))

par(mfrow = c(1, 2))
# reproducing Fig 4.13 (left)
coef.months = c(coef(mod.lm2)[2:12], -sum(coef(mod.lm2)[2:12])) # coef for December is the negative of the sum of all the others'
plot(coef.months, xlab = "Month", ylab = "Coefficient", 
     xaxt = "n", # xaxt = "n" means not drawing the x axis
     col = "blue", 
     pch = 19, type = "o")
axis(side = 1, at = 1:12, 
     labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))

# reproducing Fig 4.13 (right)
coef.hrs = coef(mod.lm2)[13:(13 + 22)]
coef.hrs = c(coef.hrs, -sum(coef.hrs))
coef.hrs
plot(coef.hrs, xlab = "Hour", ylab = "Coefficient", 
     xaxt = "n", 
     col = "blue", 
     pch = 19, type = "o")
axis(side = 1, at = c(5, 10, 15, 20))
par(mfrow = c(1, 1))

# next, try fitting with poisson regression
mod.pois = glm(
  bikers ~ mnth + hr + workingday + temp + weathersit, 
  data = Bikeshare, family = poisson # indicating to fit the glm using the eta of poisson regression, i.e. eta(Y) = log(Y)
)
summary(mod.pois)

par(mfrow = c(1, 2))
# reproducing Fig 4.15 (left)
coef.months = c(coef(mod.pois)[2:12], -sum(coef(mod.pois)[2:12])) # coef for December is the negative of the sum of all the others'
plot(coef.months, xlab = "Month", ylab = "Coefficient", 
     xaxt = "n", # xaxt = "n" means not drawing the x axis
     col = "blue", 
     pch = 19, type = "o")
axis(side = 1, at = 1:12, 
     labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))

# reproducing Fig 4.15 (right)
coef.hrs = coef(mod.pois)[13:(13 + 22)]
coef.hrs = c(coef.hrs, -sum(coef.hrs))
coef.hrs
plot(coef.hrs, xlab = "Hour", ylab = "Coefficient", 
     xaxt = "n", 
     col = "blue", 
     pch = 19, type = "o")
axis(side = 1, at = c(5, 10, 15, 20))
par(mfrow = c(1, 1))

plot(predict(mod.lm2), 
     predict(mod.pois, type = "response")) # for poisson regression, type must be specified to "response", otherwise it will output beta0 + beta1X1 instead of exp(beta0 + beta1X1)
abline(0, 1, col = 2, lwd = 3)
# the predictions from the poisson regression model are non-negative, providing more meaningful prediction for count variables