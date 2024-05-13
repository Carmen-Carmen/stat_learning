library(MASS)

csv_path = "~/Documents/stat_learning/csv_files"
setwd("~/Documents/stat_learning/chapter_3/R")
Auto = read.csv(paste(csv_path, "Auto.csv", sep = "/"), 
                na.strings = "?", # so that na.omit(Auto) can work
                stringsAsFactors = TRUE)
dim(Auto)

model = lm(mpg ~ horsepower, data = Auto)
summary(model)
# i. There is a negative relationship between horsepower and miles per gallon.
# ii. The relationship is quite strong because the p-value is very close to zero.
# iii. Negative.
?predict
predict(model, data.frame(horsepower = 98), interval = "confidence")
predict(model, data.frame(horsepower = 98), interval = "prediction")
# iv. 95% CI: [23.9731, 24.9611]
#     95% PI: [14.8049, 34.1248]

par(mfrow = c(1, 1))
plot(Auto$horsepower, Auto$mpg, pch = ".", cex = 5, 
     xlab = "horsepower", ylab = "miles per gallon")
abline(reg = model, lwd = 2, col = "red")

par(mfrow = c(2, 2))
plot(model)
# 1. the residuals vs. fitted vals graph is a U-shaped curve,
# suggesting that the relationship might not be strictly linear, 
# or that there may be some non-linear effects not captured by the model.
# 2. the residuals vs. leverage graph shows that
# there may be some influential outliers
par(mfrow = c(1, 1))

plot(Auto)
library(car)
scatterplotMatrix(Auto) # provided by car package

names(Auto)
Auto_no_names = subset(Auto, select = -name)
names(Auto_no_names)
# cor_matrix = cor(Auto_no_names, use = "complete.obs") # because horsepower col contains missing vals
Auto_no_names = na.omit(Auto_no_names) # otherwise cols containing NA vals will return NA as its correlation with other cols
cor_matrix = cor(Auto_no_names)

Auto = na.omit(Auto)
dim(Auto)
model2 = lm(mpg ~ . - name, data = Auto)
summary(model2)
# i. the F-statistic = 252.4 with a near-zero p-value, so there is a relationship
# between the response and at least 1 predictor
# ii. displacement, weight, year and origin appear to have a statistically significant relationship to the response
# iii. the coefficient = 0.75 for the year variable suggest that in the fitted model, 
# given all variables fixed, mpg increases about 0.75 units as 1 year passed by.

par(mfrow = c(2, 2))
plot(model2)
par(mfrow = c(1, 1))
# the curved residuals vs. fitted vals graph suggests that there might be a non-linear relationship
# between the fitted variables and the response
# according toe the residuals vs. leverage graph, there do exist influential outliers

model3 = lm(mpg ~ . + cylinders:weight, data = Auto_no_names)
summary(model3)
# the interaction between cylinders and weight appear to be statistically significant
# for the response mpg, though the cylinders variable itself don't.

model4 = lm(mpg ~ horsepower + I(horsepower ^ 2), data = Auto)
summary(model4)
par(mfrow = c(2, 2))
plot(model4)
par(mfrow = c(1, 1))
