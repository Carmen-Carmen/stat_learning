library(ISLR2)
library(boot)
attach(Boston)

# (a)
medv_est_mean = mean(medv)

# (b)
# sd = √var
medv_est_mean_SE = sd(medv) / sqrt(length(medv))

# (c)
boot.fn = function(data, index) {
  return(mean(data[index]))
}
boot(medv, boot.fn, R = 1000)
# Bootstrap Statistics :
#     original        bias    std. error
# t1* 22.53281 -0.0006936759   0.4154671
# the SE obtained by bootstrap is close to that of (b)

# (d)
t.test(medv) # 95 percent confidence interval: 21.72953 23.33608
# bootstrap 95 CI
22.53281 - 2 * 0.4154671 # 21.70188
22.53281 + 2 * 0.4154671 # 23.36374

# (e)
median(medv) # 21.2

# (f)
boot.fn2 = function(data, index) {
  return(median(data[index]))
}
boot(medv, boot.fn2, R = 1000)
# Bootstrap Statistics :
#     original   bias    std. error
# t1*     21.2 -0.00855   0.3872846

# (g)
quantile(medv, 0.1)

# (h)
boot.fn3 = function(data, index) {
  return(quantile(data[index], 0.1))
}
boot(medv, boot.fn3, R = 1000)
# Bootstrap Statistics :
#     original   bias    std. error
# t1*    12.75 -0.00185   0.4925004