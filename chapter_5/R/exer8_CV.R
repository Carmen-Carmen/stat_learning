set.seed(1)
x = rnorm(100)
y = x - 2 * x^2 + rnorm(100)

# (a) n is 100, and p is 1
# Y = -2X^2 + X + e

# (b)
plot(x = x, 
     y = y)
# the scatter plot is a parabola

# (c)
library(boot)
data = data.frame(
  X = x, 
  Y = y
)
set.seed(1)
for (i in 1:4) {
  fit = glm(Y ~ poly(X, i), 
            data = data, 
            family = gaussian)
  error = cv.glm(data, fit)$delta[1]
  to_print = sprintf("LOOCV error of polynomial degree %d: %.3f\n", i, error)
  cat(to_print)
}

# (d)
set.seed(999)
for (i in 1:4) {
  fit = glm(Y ~ poly(X, i), 
            data = data, 
            family = gaussian)
  error = cv.glm(data, fit)$delta[1]
  to_print = sprintf("LOOCV error of polynomial degree %d: %.3f\n", i, error)
  cat(to_print)
}
# the same with step (c), since LOOCV always fits the training set with n - 1 
# observations and test it using the remaining observation
# no variance of error rate will be introduced due to sampling

# (e)
# the model derived from formula ii has the lowest LOOCV error, which is of expectation
# since the true relationship between Y and X is quadratic

# (f)
for (i in 1:4) {
  fit = glm(Y ~ poly(X, i), 
            data = data, 
            family = gaussian)
  # error = cv.glm(data, fit)$delta[1]
  p_vals = summary(fit)$coef[-1, "Pr(>|t|)"]
  print(p_vals)
}
# the p-value of the quadratic model is the smallest