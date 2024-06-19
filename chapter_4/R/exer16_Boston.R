library(ISLR2)
library(MASS)
library(class)
library(e1071)

attach(Boston)
summary(Boston)
is_high_crime = rep(FALSE, nrow(Boston))
is_high_crime[Boston$crim >= median(Boston$crim)] = TRUE
summary(is_high_crime)

# ?Boston

par(mfrow = c(2, 2))
for (p in names(Boston)) {
  if (p != "crim") {
    plot(x = as.factor(is_high_crime), y = Boston[[p]], 
         xlab = "is high crime rate", ylab = p)
  }
}
par(mfrow = c(1, 1))

# order the predictors by their correlation with is_high_crim
predictors = Boston[,-1]
cor_list = sapply(predictors, function(x) abs(cor(x, is_high_crime)))
sorted_cor_list = sort(cor_list, decreasing = TRUE)
sorted_cor_list

Boston$is_high_crime = is_high_crime

get_random_subset_indices = function(data, subset_fraction) {
  set.seed(1)
  subset_size = round(nrow(data) * subset_fraction)
  subset_indices = sample(seq_len(nrow(data)), size = subset_size)
  return(subset_indices)
}

predictors = names(sorted_cor_list)

log_reg_pred = function(data, predictors, response) {
  fml = as.formula(paste(response, 
                         paste(predictors, collapse = "+"), sep = "~"))
  train_filter = get_random_subset_indices(data, subset_fraction = .9)
  log_reg.model = glm(fml, data = data[train_filter, ], 
                      family = binomial)
  test_set = data[-train_filter, ]
  log_reg.prob = predict(log_reg.model, test_set, type = "response")
  log_reg.pred = rep(FALSE, nrow(test_set))
  log_reg.pred[log_reg.prob > .5] = TRUE
  accuracy = mean(test_set[[response]] == log_reg.pred)
  return(accuracy)
}
log_reg_pred(Boston, predictors, "is_high_crime")

lda_pred = function(data, predictors, response) {
  fml = as.formula(paste(response, 
                         paste(predictors, collapse = "+"), sep = "~"))
  train_filter = get_random_subset_indices(data, subset_fraction = .9)
  lda.model = lda(fml, data = data, subset = train_filter)
  test_set = data[-train_filter, ]
  lda.pred = predict(lda.model, test_set)
  accuracy = mean(test_set[[response]] == lda.pred$class)
  return(accuracy)
}
lda_pred(Boston, predictors, "is_high_crime")

knn_pred = function(data, predictors, response) {
  set.seed(1)
  train_filter = get_random_subset_indices(data, subset_fraction = .9)
  X = as.data.frame(scale(data[predictors]))
  train.X = X[train_filter, , drop=FALSE]
  # train.X = scale(train.X)
  train.Y = data[train_filter, response]
  test.X = X[-train_filter, , drop=FALSE]
  # test.X = scale(test.X)
  best_k = 0
  highest_accuracy = 0
  test_set = data[-train_filter, ]
  for (k_val in 1:50) {
    knn.pred = knn(train.X, test.X, train.Y, k = k_val)
    accuracy = mean(knn.pred == test_set[[response]])
    if (accuracy > highest_accuracy) {
      highest_accuracy = accuracy
      best_k = k_val
    }
  }
  
  res = data.frame("accuracy"= highest_accuracy, 
                   "k"= best_k)
  return(res)
}
knn_pred(Boston, predictors[1:2], "is_high_crime")

nb_pred = function(data, predictors, response) {
  fml = as.formula(paste(response, 
                         paste(predictors, collapse = "+"), sep = "~"))
  train_filter = get_random_subset_indices(data, subset_fraction = .9)
  nb.model = naiveBayes(fml, data = data, subset = train_filter)
  test_set = data[train_filter, ]
  nb.pred = predict(nb.model, test_set)
  accuracy = mean(nb.pred == test_set[[response]])
  return(accuracy)
}
nb_pred(Boston, predictors, "is_high_crime")

# predictors is a vector of strings, i.e. col_names
classify_prediction = function(data, predictors, response) {
  set.seed(1)
  
  # logistic regression
  log_reg_res = log_reg_pred(data, predictors, response)
  
  # lda
  lda_res = lda_pred(data, predictors, response)
  
  # knn
  knn_res = knn_pred(data, predictors, response)$accuracy
  
  # naive Bayes
  nb_res = nb_pred(data, predictors, response)
  
  res = data.frame("log_reg" = log_reg_res, 
                   "lda" = lda_res, 
                   "knn" = knn_res, 
                   "nb" = nb_res)
  
  return(res)
}
classify_prediction(Boston, predictors, "is_high_crime")

accuracy_list = data.frame()
for (i in 1:length(predictors)) {
  predictors_temp = predictors[1:i]
  accuracy_list = rbind(accuracy_list, 
                        classify_prediction(Boston, 
                                            predictors_temp, 
                                            "is_high_crime"))
}

plot(x = 1:nrow(accuracy_list), y = accuracy_list$log_reg,
     type = "o", ylim = range(.6, 1), col = "red", 
     xlab = "number of predictors", 
     ylab = "overall accuracy")
lines(x = 1:nrow(accuracy_list), y = accuracy_list$lda, 
     type = "o", col = "blue")
lines(x = 1:nrow(accuracy_list), y = accuracy_list$knn, 
      type = "o", col = "green")
lines(x = 1:nrow(accuracy_list), y = accuracy_list$nb, 
      type = "o", col = "orange")
legend("bottomright", legend = c("logistic regression", 
                                 "linear discriminant analysis", 
                                 "k-nearest neighbors", 
                                 "naive Bayes"), 
       col = c("red", "blue", "green", "orange"), 
       lty = 1, pch = 1)

