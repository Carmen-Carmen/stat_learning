library(ISLR2)

attach(Default)
summary(Default)

set.seed(1)

# (a)
glm.fit = glm(default ~ income + balance, 
              data = Default, 
              family = binomial)
summary(glm.fit)

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
