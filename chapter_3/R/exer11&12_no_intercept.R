set.seed(1)
x = rnorm(100)
y = 2 * x + rnorm(100)

plot(x, y)

library(MASS)
model1 = lm(y ~ x + 0)
summary(model1)
# the estimated beta = 1.9939, while its std.err = 0.1065
# the t-statistic = 18.73, with a p-value < 2e-16

model2 = lm(x ~ y + 0)
summary(model2)
# the estimated beta = 0.3911, while its std.err = 0.0209
# the t-statistic = 18.73, with a p-value < 2e-16

# the t-statistics of estimated betas in model1 and model2 are identical

SE_beta = sqrt(
  sum((y - x * model1$coefficients[1]) ^ 2) / 
    (length(x) - 1) * sum(x ^ 2)
)
t_statistics = sqrt(length(x) - 1) * sum(x * y) / 
  sqrt(sum(x ^ 2) * sum(y ^ 2) - (sum(x * y)) ^ 2)

model3 = lm(y ~ x)
model4 = lm(x ~ y)
summary3 = summary(model3)
summary4 = summary(model4)
to_print = sprintf("t-statistic for y on x: %.4f\nt-statistic for x on y:%.4f", 
        summary3$coefficients[2, "t value"], 
        summary4$coefficients[2, "t value"])
cat(to_print) # print() function prints all characters

# if the true relationship between X and Y is Y = X

# the same
x = rnorm(100)
y1 = x + rnorm(100, 0, .1)
model5 = lm(y1 ~ x + 0)
model6 = lm(x ~ y1 + 0)
to_print = sprintf("y onto x: %.4f\nx onto y: %.4f", 
                   summary(model5)$coefficients[1, 1], 
                   summary(model6)$coefficients[1, 1])
cat(to_print)


# different
y2 = 10 * x + rnorm(100, 0, .1)
model7 = lm(y2 ~ x + 0)
model8 = lm(x ~ y2 + 0)
to_print = sprintf("y onto x: %.4f\nx onto y: %.4f", 
                   summary(model7)$coefficients[1, 1], 
                   summary(model8)$coefficients[1, 1])
cat(to_print)

plot(x, y1, type = "b", col = "red", ylim = range(c(y1, y2)))
lines(x, y2, type = "b", col = "blue")
