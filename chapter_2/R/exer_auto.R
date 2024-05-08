csv_path = "~/Documents/stat_learning/csv_files"
setwd("~/Documents/stat_learning/chapter_2")
Auto = read.csv(paste(csv_path, "Auto.csv", sep = "/"), 
                na.strings = "?", # so that na.omit(Auto) can work
                stringsAsFactors = TRUE)
dim(Auto)

# remove the rows with missing vals
Auto = na.omit(Auto)
dim(Auto)

# (a) Which of the predictors are quantitative, and which are qualitative?
summary(Auto)
# quantitative: mpg, displacement, horsepower, weight, acceleration
# qualitative: cylinders, year, origin, name

# (b) What is the range of each quantitative predictor? 
# You can answer this using the range() function.
range(Auto$mpg)
range(Auto$displacement)
range(Auto$horsepower)
range(Auto$weight)
range(Auto$acceleration)

# (c) What is the mean and standard deviation of each quantitative predictor?
sd(Auto$mpg)
sd(Auto$displacement)
sd(Auto$horsepower)
sd(Auto$weight)
sd(Auto$acceleration)

# (d) Now remove the 10th through 85th observations. 
# What is the range, mean, and standard deviation of each predictor 
# in the subset of the data that remains?
Auto_removed = Auto[-seq(10, 85), ]
dim(Auto_removed)
mean(Auto_removed$mpg)
mean(Auto_removed$cylinders)
mean(Auto_removed$displacement)
mean(Auto_removed$horsepower)
mean(Auto_removed$weight)
mean(Auto_removed$acceleration)
mean(Auto_removed$year)
mean(Auto_removed$origin)

# (e) Using the full data set, investigate the predictors graphically, 
# using scatterplots or other tools of your choice. 
# Create some plots highlighting the relationships among the predictors. 
# Comment on your findings.
year = as.factor(Auto$year)
plot(x = year, y = Auto$displacement, 
     varwidth = TRUE, 
     xlab = "year", ylab = "displacement")
cylinders = as.factor(Auto$cylinders)
plot(x = cylinders, y = Auto$horsepower, varwidth = TRUE)
plot(x = Auto$weight, y = Auto$acceleration)

# (f) Suppose that we wish to predict gas mileage (mpg) on the basis of 
# the other variables. Do your plots suggest that any of the other 
# variables might be useful in predicting mpg? Justify your answer.
attach(Auto)
predictors = c("cylinders", "displacement", "horsepower", "weight", "acceleration", "year")
par(mfrow = c(3, 2))
for (var in predictors) {
  if (var == "cylinders" || var == "year") {
    x = as.factor(Auto[[var]])
  } else {
    x = Auto[[var]]
  }
  y = mpg
  plot(x, y, 
       xlab = var, 
       ylab = "miles per gallon", 
       main = paste("MPG vs", var))
}
# according to the plots, mpg has negative relationships with displacement, horsepower and weight.

par(mfrow = c(1, 1))

