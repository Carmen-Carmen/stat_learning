library(ISLR2)
library(MASS)

names(Carseats)
?Carseats

model1 = lm(Sales ~ Price + Urban + US, data = Carseats)
summary(model1)
# Coefficients:
#               Estimate  Std. Error t value Pr(>|t|)    
# (Intercept)   13.043469   0.651012  20.036  < 2e-16 ***
#   Price       -0.054459   0.005242 -10.389  < 2e-16 ***
#   UrbanYes    -0.021916   0.271650  -0.081    0.936    
#   USYes        1.200573   0.259042   4.635 4.86e-06 ***
# intercept suggests that sales is not zero if price = 0, not urban, and store not in US
# Price coefficient suggests that there is a negative relationship between sales of carseats and the price
# Urban coefficient suggests that sales will not be influenced whether the store is in an urban or rural location
# US coefficient suggests that being located in US has a positive impact on sales of carseats of the store

# Y = 13.04 - 0.0545 * Price - 0.0219 + 1.2001 ==> in urban and in US
# Y = 13.04 - 0.0535 * Price + 1.2001 ==> not in urban but in US
# Y = 13.04 - 0.0535 * Price - 0.0219 ==> not in US but in urban
# Y = 13.04 - 0.0353 * Price ==> neither in urban nor in US

# for the predictor Urban, H0 can be rejected, since the p-value for it is close to 1

model2 = lm(Sales ~ Price + US, data = Carseats)
summary(model2)

summary(model1)
summary(model2)
# they have similar R-squared, with a slight improvement in model2
anova(model1, model2)
# anova() suggests no significant superior of model2 over model1, and vice versa

confint(model2)
#                   2.5 %      97.5 %
# (Intercept) 11.79032020 14.27126531
# Price       -0.06475984 -0.04419543
# USYes        0.69151957  1.70776632

par(mfrow = c(2, 2))
plot(model2)
# some outliers can be found in the residuals vs. leverage graph
par(mfrow = c(1, 1))
