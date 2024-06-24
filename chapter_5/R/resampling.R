library(ISLR2)

setwd("~/Documents/stat_learning/chapter_5/R")

# 5.3.1 The Validation Set Approach
set.seed(1)
train = sample(392, 196)

lm.fit = lm(mpg ~ horsepower, data = Auto, subset = train)
attach(Auto)

# calculate MSE with the linear model
lm.MSE = mean(((mpg - predict(lm.fit, Auto))[-train]) ^ 2)
lm.MSE # 23.26601

lm.fit2 = lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
# calculate MSE with the quadratic model
lm.MSE2 = mean(((mpg - predict(lm.fit2, Auto))[-train]) ^ 2)
lm.MSE2 # 18.71646

lm.fit3 = lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
summary(lm.fit3)
# MSE with the cubic model
lm.MSE3 = mean(((mpg - predict(lm.fit3, Auto))[-train]) ^ 2)
lm.MSE3 # 18.79401

# try on a different training set 
set.seed(2)
train = sample(392, 196) # from 392 observations, randomly choose 196 as the training set
lm.fit = lm(mpg ~ horsepower, data = Auto, subset = train)
lm.fit2 = lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
lm.fit3 = lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
lm.MSE = mean(((mpg - predict(lm.fit, Auto))[-train]) ^ 2)
lm.MSE2 = mean(((mpg - predict(lm.fit2, Auto))[-train]) ^ 2)
lm.MSE3 = mean(((mpg - predict(lm.fit3, Auto))[-train]) ^ 2)
to_print = sprintf("linear MSE: %.2f\nquadratic MSE: %.2f\ncubic MSE: %.2f", lm.MSE, lm.MSE2, lm.MSE3)
cat(to_print) # use concatenate to output strings containing "\n"

# redraw the plot on the textbook
poly_num = c(seq(1, 10))
validation_set_df = data.frame(poly_num)

for (i in seq(1, 10)) {
  set.seed(i)
  train = sample(nrow(Auto), nrow(Auto) / 2)
  MSE_list = c()
  for (k in poly_num) {
    # print(k)
    lm.fit = lm(mpg ~ poly(horsepower, k), data = Auto, subset = train)
    lm.MSE = mean(((mpg - predict(lm.fit, Auto))[-train]) ^ 2)
    MSE_list = c(MSE_list, lm.MSE)
  }
  validation_set_df = cbind(validation_set_df, MSE_list)
}

plot(x = validation_set_df[,1], y = validation_set_df[,2], 
     xlab = "polynomial numbers", ylab = "MSE", type = "o", 
     ylim = range(15, 30))
for (i in seq(3, 10)) {
  lines(x = validation_set_df[,1], y = validation_set_df[,i], 
        xlab = "polynomial numbers", ylab = "MSE", type = "o")
}

# 5.3.2 Leave-One-Out Cross-Validation, LOOCV
set.seed(2)
glm.fit = glm(mpg ~ horsepower, data = Auto,)
coef(glm.fit)
# (Intercept)  horsepower 
# 39.9358610  -0.1578447 
lm.fit = lm(mpg ~ horsepower, data = Auto)
coef(lm.fit)
# (Intercept)  horsepower 
# 39.9358610  -0.1578447 

library(boot)
cv.err = cv.glm(Auto, glm.fit)
cv.err$delta # 24.23151 24.23114
# delta vector contains the cross-validation results, i.e. estimates for the test errors

cv.errors = rep(0, 10)
for (i in 1 : 10) {
  glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.errors[i] = cv.glm(Auto, glm.fit)$delta[1]
}
cv.errors # 24.23151 19.24821 19.33498 19.42443 19.03321 18.97864 18.83305 18.96115 19.06863 19.49093

# 5.3.3 K-Fold Cross-Validation
set.seed(17)
cv.error.10 = rep(0, 10)
for (i in seq(1, 10)) {
  glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error.10[i] = cv.glm(Auto, glm.fit, K = 10)$delta[1]
  print(cv.glm(Auto, glm.fit, K = 10)$delta)
  # the first number is the standard k-fold CV estimate
  # while the second is a bias-corrected one, slightly different
  # [1] 24.08261 24.07523
  # [1] 19.14276 19.13436
  # [1] 19.31430 19.29461
  # [1] 19.51186 19.47755
  # [1] 19.19115 19.14911
  # [1] 18.98648 18.94621
  # [1] 18.78177 18.74373
  # [1] 18.87421 18.83082
  # [1] 19.11015 19.05006
  # [1] 19.0269 18.9629
}
cv.error.10 # 24.27207 19.26909 19.34805 19.29496 19.03198 18.89781 19.12061 19.14666 18.87013 20.95520

# 5.3.4 The Bootstrap
# One of the great advantages of the bootstrap approach is that 
# it can be applied in almost all situations.
# step 1: create a function that computes the statistic of interest
# take the investment returns as example
alpha.fn = function(data, index) {
  X = data$X[index]
  Y = data$Y[index]
  (var(Y) - cov(X, Y)) / (var(X) + var(Y) - 2 * cov(X, Y))
}
alpha.fn(Portfolio, 1:100) # 0.5758321
# randomly select 100 observations from the range 1 to 100, with replacement
# equivalent to constructing a new bootstrap data set and recomputing ^α based on the new data set
set.seed(7)
alpha.fn(Portfolio, sample(100, 100, replace = T)) # 0.5385326

# implement the command by multiple times
boot(Portfolio, alpha.fn, R = 1000)
# ORDINARY NONPARAMETRIC BOOTSTRAP
# 
# Call:
# boot(data = Portfolio, statistic = alpha.fn, R = 1000)
# 
# Bootstrap Statistics :
#       original       bias    std. error
# t1* 0.5758321 0.0007959475  0.08969074

# Estimating the Accuracy of a Linear Regression Model
boot.fn = function(data, index) {
  coef(lm(mpg ~ horsepower, data = data, subset = index))
}
boot.fn(Auto, 1:392)
# (Intercept)  horsepower 
# 39.9358610  -0.1578447

set.seed(1)
boot.fn(Auto, sample(392, 392, replace = T))
# (Intercept)  horsepower 
# 40.3404517  -0.1634868
boot.fn(Auto, sample(392, 392, replace = T))
# (Intercept)  horsepower 
# 40.1186906  -0.1577063

# compute the errors of 100 bootstrap estimates
boot(Auto, boot.fn, 1000)
# ORDINARY NONPARAMETRIC BOOTSTRAP
# 
# Call:
#   boot(data = Auto, statistic = boot.fn, R = 1000)
# 
# Bootstrap Statistics :
#       original        bias    std. error
# t1* 39.9358610  0.0544513229 0.841289790
# t2* -0.1578447 -0.0006170901 0.007343073
summary(lm(mpg ~ horsepower, data = Auto))$coef
#               Estimate  Std. Error   t value      Pr(>|t|)
# (Intercept) 39.9358610 0.717498656  55.65984 1.220362e-187
# horsepower  -0.1578447 0.006445501 -24.48914  7.031989e-81

# use bootstrap to estimate the SE of coefs of a quadratic model
boot.fn2 = function(data, index) {
  coef(lm(mpg ~ poly(horsepower, 2), data = data, subset = index))
}
set.seed(1)
boot(Auto, boot.fn2, 1000)
# ORDINARY NONPARAMETRIC BOOTSTRAP
# 
# Call:
# boot(data = Auto, statistic = boot.fn2, R = 1000)
# 
# Bootstrap Statistics :
#       original       bias    std. error
# t1*   23.44592 -0.003660358   0.2195369
# t2* -120.13774  0.002769239   3.6138046
# t3*   44.08953  0.101767465   4.1998076
summary(
  lm(mpg ~ poly(horsepower, 2), data = Auto)
)$coef
#                       Estimate Std. Error   t value      Pr(>|t|)
# (Intercept)            23.44592  0.2209163 106.13030 2.752212e-289
# poly(horsepower, 2)1 -120.13774  4.3739206 -27.46683  4.169400e-93
# poly(horsepower, 2)2   44.08953  4.3739206  10.08009  2.196340e-21