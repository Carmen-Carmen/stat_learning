library(ISLR2)
Boston
?Boston

# (a) How many rows are in this data set? How many columns? 
# What do the rows and columns represent?
dim(Boston)
summary(Boston)
# there are 506 rows, and each row represents a suburb of Boston
# each col represents an attribute of the suburbs

# (b) Make some pairwise scatterplots of the predictors (columns) in this data set. 
# Describe your findings.
pairs(Boston)

# (c) Are any of the predictors associated with per capita crime rate? 
# If so, explain the relationship.
predictors = names(Boston)
predictors = predictors[-1] # remove crime
for (var in predictors) {
  x = Boston[[var]]
  y = Boston[["crim"]]
  correlation = cor(x, y)
  if (correlation > 0.5) {
    to_print = sprintf("correlation between %s and %s: %.2f", var, "crim", correlation)
    print(to_print)
  }
}
# [1] "correlation between rad and crim: 0.63"
# [1] "correlation between tax and crim: 0.58"

# (d) Do any of the census tracts of Boston appear to have particularly high 
# crime rates? Tax rates? Pupil-teacher ratios? Comment on the range of each predictor.
attach(Boston)
range(crim)
range(tax)
range(ptratio)

# (e) How many of the census tracts in this data set bound the Charles river?
print(sum(Boston$chas == 1))

# (f) What is the median pupil-teacher ratio among the towns in this data set?
print(median(Boston[["ptratio"]]))

# (g) Which census tract of Boston has lowest median value of owner-occupied homes? 
# What are the values of the other predictors for that census tract, 
# and how do those values compare to the overall ranges for those predictors? 
# Comment on your findings.
min_index = which.min(Boston$medv)
Boston[min_index, ]
for (var in predictors) {
  if (var == "medv") {
    next
  }
  val = Boston[[var]][min_index]
  mean_val = mean(Boston[[var]])
  to_print = ""
  if (val > mean_val) {
    to_print = sprintf("[%s] = %f higher than mean: %f", var, val, mean_val)
  } else if (val == mean_val){
    to_print = sprintf("[%s] = %f equals mean: %f", var, val, mean_val)
  } else {
    to_print = sprintf("[%s] = %f lower than mean: %f", var, val, mean_val)
  }
  print(to_print)
}
# [1] "[zn] lower than mean: 11.363636"
# [1] "[indus] higher than mean: 11.136779"
# [1] "[chas] lower than mean: 0.069170"
# [1] "[nox] higher than mean: 0.554695"
# [1] "[rm] lower than mean: 6.284634"
# [1] "[age] higher than mean: 68.574901"
# [1] "[dis] lower than mean: 3.795043"
# [1] "[rad] higher than mean: 9.549407"
# [1] "[tax] higher than mean: 408.237154"
# [1] "[ptratio] higher than mean: 18.455534"
# [1] "[lstat] higher than mean: 12.653063"

# (h) In this data set, how many of the census tracts average 
# more than seven rooms per dwelling? More than eight rooms per dwelling? 
# Comment on the census tracts that average more than eight rooms per dwelling.
more_than_7 = Boston[Boston$rm > 7, ]
more_than_8 = Boston[Boston$rm > 8, ]
summary(more_than_7)
summary(more_than_8)
