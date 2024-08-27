library(ISLR2)

attach(Default)
summary(Default)

set.seed(1)

# (a)
glm.fit = glm(default ~ income + balance, 
              data = Default, 
              family = binomial)
summary(glm.fit)
# Coefficients:
#                 Estimate Std. Error z value Pr(>|z|)    
#   (Intercept) -1.154e+01  4.348e-01 -26.545  < 2e-16 ***
#   income       2.081e-05  4.985e-06   4.174 2.99e-05 ***
#   balance      5.647e-03  2.274e-04  24.836  < 2e-16 ***

nrow(Default)
# (b) validation set approach
train_filter = sample(nrow(Default), nrow(Default) / 2)
glm.train = glm(default ~ income + balance, 
                data = Default, 
                subset = train_filter, 
                family = binomial)
glm.prob_list = predict(glm.train, Default[-train_filter, ], type = "response")
Default$default
glm.pred = rep("No", nrow(Default) - length(train_filter))
glm.pred[glm.prob_list > .5] = "Yes"
glm.pred
validation_set_error_fraction = sum(glm.pred != Default$default[-train_filter]) / length(glm.pred)
validation_set_error_fraction # 0.0254

# (c) repeat (b) 3 times using 3 different splits
for (i in 2:4) {
  set.seed(i)
  train_filter = sample(nrow(Default), nrow(Default) / 2)
  glm.train = glm(default ~ income + balance, 
                  data = Default, 
                  subset = train_filter, 
                  family = binomial)
  glm.prob_list = predict(glm.train, Default[-train_filter, ], type = "response")
  Default$default
  glm.pred = rep("No", nrow(Default) - length(train_filter))
  glm.pred[glm.prob_list > .5] = "Yes"
  glm.pred
  validation_set_error_fraction = sum(glm.pred != Default$default[-train_filter]) / length(glm.pred)
  print(validation_set_error_fraction)
}
# [1] 0.0238
# [1] 0.0264
# [1] 0.0256

# (d) include the "student" dummy variable
set.seed(1)
train_filter = sample(nrow(Default), nrow(Default) / 2)
glm.train = glm(default ~ income + balance + student, 
                data = Default, 
                subset = train_filter, 
                family = binomial)
glm.prob_list = predict(glm.train, Default[-train_filter, ], type = "response")
Default$default
glm.pred = rep("No", nrow(Default) - length(train_filter))
glm.pred[glm.prob_list > .5] = "Yes"
glm.pred
validation_set_error_fraction = sum(glm.pred != Default$default[-train_filter]) / length(glm.pred)
print(validation_set_error_fraction) # 0.026
# not significantly smaller than the model of "default ~ income + balance"
