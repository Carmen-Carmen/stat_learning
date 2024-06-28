library(ISLR2)

set.seed(1)

attach(Default)

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
coefs = summary(glm.fit)$coef
coefs["income", 2] # 4.985167e-06
coefs["balance", 2] # 0.0002273731

# (b)
boot.fn = function(data, index) {
  fit = glm(default ~ income + balance, 
            data = data, 
            subset = index, 
            family = binomial)
  sds = coef(fit)[-1]
  return(sds)
}
# boot.fn(Default, sample(nrow(Default), nrow(Default), replace = T))

# (c)
library(boot)
boot(Default, boot.fn, R = 1000)
# Bootstrap Statistics :
#         original       bias     std. error
# t1* 2.080898e-05 1.264142e-08 4.767307e-06
# t2* 5.647103e-03 1.700319e-05 2.362239e-04

# (d)
# the SD obtained by bootstrap is relatively close to the glm() function