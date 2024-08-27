library(ISLR2)

# best subset selection
names(Hitters)
dim(Hitters) # 322  20

sum(is.na(Hitters$Salary)) # 59
Hitters = na.omit(Hitters) # remove the missing rows
dim(Hitters) # 322  20

library(leaps)
regfit.full = regsubsets(Salary ~ ., Hitters)
summary(regfit.full)
# Selection Algorithm: exhaustive
#         AtBat Hits HmRun Runs RBI Walks Years CAtBat CHits CHmRun CRuns CRBI CWalks
# 1  ( 1 ) " "   " "  " "   " "  " " " "   " "   " "    " "   " "    " "   "*"  " "   
# 2  ( 1 ) " "   "*"  " "   " "  " " " "   " "   " "    " "   " "    " "   "*"  " "   
# 3  ( 1 ) " "   "*"  " "   " "  " " " "   " "   " "    " "   " "    " "   "*"  " "   
# 4  ( 1 ) " "   "*"  " "   " "  " " " "   " "   " "    " "   " "    " "   "*"  " "   
# 5  ( 1 ) "*"   "*"  " "   " "  " " " "   " "   " "    " "   " "    " "   "*"  " "   
# 6  ( 1 ) "*"   "*"  " "   " "  " " "*"   " "   " "    " "   " "    " "   "*"  " "   
# 7  ( 1 ) " "   "*"  " "   " "  " " "*"   " "   "*"    "*"   "*"    " "   " "  " "   
# 8  ( 1 ) "*"   "*"  " "   " "  " " "*"   " "   " "    " "   "*"    "*"   " "  "*"   
#         LeagueN DivisionW PutOuts Assists Errors NewLeagueN
# 1  ( 1 ) " "     " "       " "     " "     " "    " "       
# 2  ( 1 ) " "     " "       " "     " "     " "    " "       
# 3  ( 1 ) " "     " "       "*"     " "     " "    " "       
# 4  ( 1 ) " "     "*"       "*"     " "     " "    " "       
# 5  ( 1 ) " "     "*"       "*"     " "     " "    " "       
# 6  ( 1 ) " "     "*"       "*"     " "     " "    " "       
# 7  ( 1 ) " "     "*"       "*"     " "     " "    " "       
# 8  ( 1 ) " "     "*"       "*"     " "     " "    " "  
# An asterisk indixates that a given predictor is included in the corresponding model

regfit.full = regsubsets(Salary ~ ., data = Hitters, 
                         nvmax = 19) # specifying the max number of variables included
summary(regfit.full)
reg.summary = summary(regfit.full)
names(reg.summary)
# [1] "which"  "rsq"    "rss"    "adjr2"  "cp"     "bic"    "outmat" "obj" 
reg.summary$rsq # fetch the R-squared of the fitted models

# plotting of the parameters
par(mfrow = c(2, 2))
# RSS
plot(reg.summary$rss, xlab = "Number of variables included", 
     ylab = "RSS", type = "l")
# Adjusted R-squared
plot(reg.summary$adjr2, xlab = "Number of variables included",
     ylab = "Adjusted RSq", type = "l")
which.max(reg.summary$adjr2) # 11
points(11, reg.summary$adjr2[11], 
       col = "red", cex = 2, pch = 20) # point out the largest adjusted RSq
# Cp
plot(reg.summary$cp, xlab = "Number of variables included", 
     ylab = "Cp", type = "l")
which.min(reg.summary$cp) # 10
points(11, reg.summary$cp[10], 
       col = "red", cex = 2, pch = 20)
# BIC
which.min(reg.summary$bic) # 6
plot(reg.summary$bic, xlab = "Number of variables included", 
     ylab = "BIC", type = "l")
points(6, reg.summary$bic[6], col = "red", cex = 2, pch = 20)

# the regsubsets() function also providesa built-in plot() to display the 
# selected variables for the best model, ranked according to BIC, Cp, adjusted R^2 or AIC
?plot.regsubsets
par(mfrow = c(1, 1))
plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2" )
plot(regfit.full, scale = "Cp" )
plot(regfit.full, scale = "bic" )
# the top row of each plot contains a black square for each variable selected
# according to the optimal model associated with that statistic
coef(regfit.full, 6)
# fetch the 6 coefficients of the best model with smallest BIC
# (Intercept)        AtBat         Hits        Walks         CRBI    DivisionW 
# 91.5117981   -1.8685892    7.6043976    3.6976468    0.6430169 -122.9515338 
# PutOuts 
# 0.2643076

# Forward and Backward Stepwise Selection
# using regsubsets()
regfit.fwd = regsubsets(Salary ~ ., data = Hitters, 
                        nvmax = 19, method = "forward")
summary(regfit.fwd)
# Selection Algorithm: forward
#           AtBat Hits HmRun Runs RBI Walks Years CAtBat CHits CHmRun CRuns CRBI CWalks
# 1  ( 1 )  " "   " "  " "   " "  " " " "   " "   " "    " "   " "    " "   "*"  " "   
# 2  ( 1 )  " "   "*"  " "   " "  " " " "   " "   " "    " "   " "    " "   "*"  " "   
# 3  ( 1 )  " "   "*"  " "   " "  " " " "   " "   " "    " "   " "    " "   "*"  " "   
# 4  ( 1 )  " "   "*"  " "   " "  " " " "   " "   " "    " "   " "    " "   "*"  " "   
# 5  ( 1 )  "*"   "*"  " "   " "  " " " "   " "   " "    " "   " "    " "   "*"  " "   
# 6  ( 1 )  "*"   "*"  " "   " "  " " "*"   " "   " "    " "   " "    " "   "*"  " "   
# 7  ( 1 )  "*"   "*"  " "   " "  " " "*"   " "   " "    " "   " "    " "   "*"  "*"   
# 8  ( 1 )  "*"   "*"  " "   " "  " " "*"   " "   " "    " "   " "    "*"   "*"  "*"   
# 9  ( 1 )  "*"   "*"  " "   " "  " " "*"   " "   "*"    " "   " "    "*"   "*"  "*"   
# 10  ( 1 ) "*"   "*"  " "   " "  " " "*"   " "   "*"    " "   " "    "*"   "*"  "*"   
# 11  ( 1 ) "*"   "*"  " "   " "  " " "*"   " "   "*"    " "   " "    "*"   "*"  "*"   
# 12  ( 1 ) "*"   "*"  " "   "*"  " " "*"   " "   "*"    " "   " "    "*"   "*"  "*"   
# 13  ( 1 ) "*"   "*"  " "   "*"  " " "*"   " "   "*"    " "   " "    "*"   "*"  "*"   
# 14  ( 1 ) "*"   "*"  "*"   "*"  " " "*"   " "   "*"    " "   " "    "*"   "*"  "*"   
# 15  ( 1 ) "*"   "*"  "*"   "*"  " " "*"   " "   "*"    "*"   " "    "*"   "*"  "*"   
# 16  ( 1 ) "*"   "*"  "*"   "*"  "*" "*"   " "   "*"    "*"   " "    "*"   "*"  "*"   
# 17  ( 1 ) "*"   "*"  "*"   "*"  "*" "*"   " "   "*"    "*"   " "    "*"   "*"  "*"   
# 18  ( 1 ) "*"   "*"  "*"   "*"  "*" "*"   "*"   "*"    "*"   " "    "*"   "*"  "*"   
# 19  ( 1 ) "*"   "*"  "*"   "*"  "*" "*"   "*"   "*"    "*"   "*"    "*"   "*"  "*"   
#         LeagueN DivisionW PutOuts Assists Errors NewLeagueN
# 1  ( 1 )  " "     " "       " "     " "     " "    " "       
# 2  ( 1 )  " "     " "       " "     " "     " "    " "       
# 3  ( 1 )  " "     " "       "*"     " "     " "    " "       
# 4  ( 1 )  " "     "*"       "*"     " "     " "    " "       
# 5  ( 1 )  " "     "*"       "*"     " "     " "    " "       
# 6  ( 1 )  " "     "*"       "*"     " "     " "    " "       
# 7  ( 1 )  " "     "*"       "*"     " "     " "    " "       
# 8  ( 1 )  " "     "*"       "*"     " "     " "    " "       
# 9  ( 1 )  " "     "*"       "*"     " "     " "    " "       
# 10  ( 1 ) " "     "*"       "*"     "*"     " "    " "       
# 11  ( 1 ) "*"     "*"       "*"     "*"     " "    " "       
# 12  ( 1 ) "*"     "*"       "*"     "*"     " "    " "       
# 13  ( 1 ) "*"     "*"       "*"     "*"     "*"    " "       
# 14  ( 1 ) "*"     "*"       "*"     "*"     "*"    " "       
# 15  ( 1 ) "*"     "*"       "*"     "*"     "*"    " "       
# 16  ( 1 ) "*"     "*"       "*"     "*"     "*"    " "       
# 17  ( 1 ) "*"     "*"       "*"     "*"     "*"    "*"       
# 18  ( 1 ) "*"     "*"       "*"     "*"     "*"    "*"       
# 19  ( 1 ) "*"     "*"       "*"     "*"     "*"    "*"    
regfit.bwd = regsubsets(Salary ~ ., data = Hitters, 
                        nvmax = 19, method = "backward")
summary(regfit.bwd)
# Selection Algorithm: backward
#         AtBat Hits HmRun Runs RBI Walks Years CAtBat CHits CHmRun CRuns CRBI CWalks
# 1  ( 1 )  " "   " "  " "   " "  " " " "   " "   " "    " "   " "    "*"   " "  " "   
# 2  ( 1 )  " "   "*"  " "   " "  " " " "   " "   " "    " "   " "    "*"   " "  " "   
# 3  ( 1 )  " "   "*"  " "   " "  " " " "   " "   " "    " "   " "    "*"   " "  " "   
# 4  ( 1 )  "*"   "*"  " "   " "  " " " "   " "   " "    " "   " "    "*"   " "  " "   
# 5  ( 1 )  "*"   "*"  " "   " "  " " "*"   " "   " "    " "   " "    "*"   " "  " "   
# 6  ( 1 )  "*"   "*"  " "   " "  " " "*"   " "   " "    " "   " "    "*"   " "  " "   
# 7  ( 1 )  "*"   "*"  " "   " "  " " "*"   " "   " "    " "   " "    "*"   " "  "*"   
# 8  ( 1 )  "*"   "*"  " "   " "  " " "*"   " "   " "    " "   " "    "*"   "*"  "*"   
# 9  ( 1 )  "*"   "*"  " "   " "  " " "*"   " "   "*"    " "   " "    "*"   "*"  "*"   
# 10  ( 1 ) "*"   "*"  " "   " "  " " "*"   " "   "*"    " "   " "    "*"   "*"  "*"   
# 11  ( 1 ) "*"   "*"  " "   " "  " " "*"   " "   "*"    " "   " "    "*"   "*"  "*"   
# 12  ( 1 ) "*"   "*"  " "   "*"  " " "*"   " "   "*"    " "   " "    "*"   "*"  "*"   
# 13  ( 1 ) "*"   "*"  " "   "*"  " " "*"   " "   "*"    " "   " "    "*"   "*"  "*"   
# 14  ( 1 ) "*"   "*"  "*"   "*"  " " "*"   " "   "*"    " "   " "    "*"   "*"  "*"   
# 15  ( 1 ) "*"   "*"  "*"   "*"  " " "*"   " "   "*"    "*"   " "    "*"   "*"  "*"   
# 16  ( 1 ) "*"   "*"  "*"   "*"  "*" "*"   " "   "*"    "*"   " "    "*"   "*"  "*"   
# 17  ( 1 ) "*"   "*"  "*"   "*"  "*" "*"   " "   "*"    "*"   " "    "*"   "*"  "*"   
# 18  ( 1 ) "*"   "*"  "*"   "*"  "*" "*"   "*"   "*"    "*"   " "    "*"   "*"  "*"   
# 19  ( 1 ) "*"   "*"  "*"   "*"  "*" "*"   "*"   "*"    "*"   "*"    "*"   "*"  "*"   
#         LeagueN DivisionW PutOuts Assists Errors NewLeagueN
# 1  ( 1 )  " "     " "       " "     " "     " "    " "       
# 2  ( 1 )  " "     " "       " "     " "     " "    " "       
# 3  ( 1 )  " "     " "       "*"     " "     " "    " "       
# 4  ( 1 )  " "     " "       "*"     " "     " "    " "       
# 5  ( 1 )  " "     " "       "*"     " "     " "    " "       
# 6  ( 1 )  " "     "*"       "*"     " "     " "    " "       
# 7  ( 1 )  " "     "*"       "*"     " "     " "    " "       
# 8  ( 1 )  " "     "*"       "*"     " "     " "    " "       
# 9  ( 1 )  " "     "*"       "*"     " "     " "    " "       
# 10  ( 1 ) " "     "*"       "*"     "*"     " "    " "       
# 11  ( 1 ) "*"     "*"       "*"     "*"     " "    " "       
# 12  ( 1 ) "*"     "*"       "*"     "*"     " "    " "       
# 13  ( 1 ) "*"     "*"       "*"     "*"     "*"    " "       
# 14  ( 1 ) "*"     "*"       "*"     "*"     "*"    " "       
# 15  ( 1 ) "*"     "*"       "*"     "*"     "*"    " "       
# 16  ( 1 ) "*"     "*"       "*"     "*"     "*"    " "       
# 17  ( 1 ) "*"     "*"       "*"     "*"     "*"    "*"       
# 18  ( 1 ) "*"     "*"       "*"     "*"     "*"    "*"       
# 19  ( 1 ) "*"     "*"       "*"     "*"     "*"    "*"

# Choosing Among Models Using the Validation-Set Approach and Cross-Validation
# use only the training observations to perform all aspects of model-fitting, 
# including the variable selection progress
# validation-set approach
set.seed(1)
train = sample(c(TRUE, FALSE), nrow(Hitters), replace = TRUE)
test = (!train)
regfit.best = regsubsets(Salary ~ ., 
                         data = Hitters[train, ], nvmax = 19)
# model.matrix, used for building an "X" matrix from data
test.matrix = model.matrix(Salary ~ ., data = Hitters[test, ])
val.errors = rep(NA, 19)
for (i in 1:19) {
  coefi = coef(regfit.best, id = i)
  # to multiply the value of variables with its corresponding coef
  pred = test.matrix[, names(coefi)] %*% coefi # %*% means matrix multiplication
  val.errors[i] = mean((Hitters$Salary[test] - pred) ^ 2)
}
val.errors
which.min(val.errors)

regfit.best = regsubsets(Salary ~ ., data = Hitters, nvmax = 19)
coef(regfit.best, 7)
# (Intercept)         Hits        Walks       CAtBat        CHits       CHmRun 
# 79.4509472    1.2833513    3.2274264   -0.3752350    1.4957073    1.4420538 
# DivisionW      PutOuts 
# -129.9866432    0.2366813

# cross-validation approach
k = 10
n = nrow(Hitters)
set.seed(1)
folds = sample(rep(1:k, length = n))
# cv.errors is a 10-row 19-col matrix
cv.errors = matrix(NA, k, 19, 
                   dimnames = list(NULL, paste(1: 19)))

predict.regsubsets = function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  xvars = names(coefi)
  pred = mat[, xvars] %*% coefi
  return(pred)
}

for (j in 1:k) {
  best.fit = regsubsets(Salary ~ ., 
                        data = Hitters[folds != j, ], 
                        nvmax = 19)
  for (i in 1:19) {
    # the predict.regsubsets will be automatically called
    # because the best.fit object has class regsubsets
    # pred = predict(best.fit, Hitters[folds == j, ], id = i)
    pred = predict.regsubsets(best.fit, Hitters[folds == j, ], id = i) # also worked
    cv.errors[j, i] = mean((Hitters$Salary[folds == j] - pred) ^ 2)
  }
}

mean.cv.errors = apply(cv.errors, 2, mean)
mean.cv.errors
# 1        2        3        4        5        6        7        8        9 
# 143439.8 126817.0 134214.2 131782.9 130765.6 120382.9 121443.1 114363.7 115163.1 
# 10       11       12       13       14       15       16       17       18 
# 109366.0 112738.5 113616.5 115557.6 115853.3 115630.6 116050.0 116117.0 116419.3 
# 19 
# 116299.1 
plot(mean.cv.errors, type = "b")
reg.best = regsubsets(Salary ~ ., data = Hitters, nvmax = 19)
coef(reg.best, 10)
# (Intercept)        AtBat         Hits        Walks       CAtBat        CRuns 
# 162.5354420   -2.1686501    6.9180175    5.7732246   -0.1300798    1.4082490 
# CRBI       CWalks    DivisionW      PutOuts      Assists 
# 0.7743122   -0.8308264 -112.3800575    0.2973726    0.2831680

# Ridge Regression and the Lasso
# an x matrix and a y vector need to be passed to glmnet()
# similar to the scenario when fitting linear models with python
x = model.matrix(Salary ~ ., Hitters)[, -1] # remove the intercept
# In techniques like ridge regression or lasso, it's common to remove the 
# intercept from the matrix to prevent it from 
# being regularized (penalized), allowing it to be estimated separately.
y = Hitters$Salary

# ridge repression
library(glmnet)
grid = 10^seq(10, -2, length = 100) # lambda ranging from 10^-2 to 10^10
ridge.mod = glmnet(x, y, alpha = 0, lambda = grid)
dim(coef(ridge.mod))
ridge.mod$lambda[50] # 11498
coef(ridge.mod)[, 50] # check the coefficients when lambda = 11498
# (Intercept)         AtBat          Hits         HmRun          Runs           RBI 
# 407.356050200   0.036957182   0.138180344   0.524629976   0.230701523   0.239841459 
# Walks         Years        CAtBat         CHits        CHmRun         CRuns 
# 0.289618741   1.107702929   0.003131815   0.011653637   0.087545670   0.023379882 
# CRBI        CWalks       LeagueN     DivisionW       PutOuts       Assists 
# 0.024138320   0.025015421   0.085028114  -6.215440973   0.016482577   0.002612988 
# Errors    NewLeagueN 
# -0.020502690   0.301433531
sqrt(sum(coef(ridge.mod)[-1, 50] ^ 2)) # 6.36, the L2 norm, with intercept removed

ridge.mod$lambda[60] # lambda = 705
coef(ridge.mod)[, 60]
sqrt(sum(coef(ridge.mod)[-1, 60] ^ 2)) # L2 norm = 57.1

# use the predict() function to obtain coefficients for specified lambda values
predict(ridge.mod, s = 50, type = "coefficients")[1:20, ]
# (Intercept)       AtBat        Hits       HmRun        Runs         RBI       Walks 
# 4.877e+01  -3.581e-01   1.969e+00  -1.278e+00   1.146e+00   8.038e-01   2.716e+00 
# Years      CAtBat       CHits      CHmRun       CRuns        CRBI      CWalks 
# -6.218e+00   5.448e-03   1.065e-01   6.245e-01   2.215e-01   2.187e-01  -1.500e-01 
# LeagueN   DivisionW     PutOuts     Assists      Errors  NewLeagueN 
# 4.593e+01  -1.182e+02   2.502e-01   1.216e-01  -3.279e+00  -9.497e+00 

# estimate the test error of ridge regression by validation-set approach
set.seed(1)
train = sample(1:nrow(x), nrow(x) / 2)
test = (-train)
y.test = y[test]
ridge.mod = glmnet(x[train, ], y[train], alpha = 0, 
                   lambda = grid, thresh = 1e-12)
ridge.pred = predict(ridge.mod, s = 4, newx = x[test, ]) # test using lambda = 4
mean((ridge.pred - y.test) ^ 2) # 142199

# if instead fit a model with just an intercept, 
# i.e. predict each test observation using the mean of training observations
mean((mean(y[train]) - y.test) ^ 2) # 224670
# the same result with a very large value of lambda
# i.e. all variables are excluded from the model
ridge.pred = predict(ridge.mod, s = 1e10, newx = x[test, ])
mean((ridge.pred - y.test) ^ 2) # 224670

# OLS is simply performing ridge regression with lambda = 0
ridge.pred = predict(ridge.mod, s = 0, newx = x[test, ], 
                     exact = T, x = x[train, ], y = y[train])
mean((ridge.pred - y.test) ^ 2) # 168589
lm(y ~ x, subset = train)
# Call:
#   lm(formula = y ~ x, subset = train)
# 
# Coefficients:
#   (Intercept)       xAtBat        xHits       xHmRun        xRuns         xRBI  
#       274.014       -0.352       -1.638        5.814        1.542        1.124  
# xWalks       xYears      xCAtBat       xCHits      xCHmRun       xCRuns  
# 3.729      -16.377       -0.641        3.163        3.401       -0.974  
# xCRBI      xCWalks     xLeagueN   xDivisionW     xPutOuts     xAssists  
# -0.601        0.338      119.149     -144.083        0.198        0.680  
# xErrors  xNewLeagueN  
# -4.713      -71.095 
predict(ridge.mod, s = 0, exact = T, type = "coefficients", 
        x = x[train, ], y = y[train])[1:20, ]
# (Intercept)       AtBat        Hits       HmRun        Runs         RBI       Walks 
# 274.0201     -0.3522     -1.6371      5.8147      1.5423      1.1242      3.7288 
# Years      CAtBat       CHits      CHmRun       CRuns        CRBI      CWalks 
# -16.3795     -0.6411      3.1629      3.4005     -0.9739     -0.6004      0.3378 
# LeagueN   DivisionW     PutOuts     Assists      Errors  NewLeagueN 
# 119.1435   -144.0853      0.1976      0.6804     -4.7128    -71.0899 
# "exact = T" is used to yield the exact least square coefficients when calling predict()
# otherwise the predict() function will give approximate results, 
# though there is still a slite discrepancy between the output of glmnet() and lm()

# the CV approach
set.seed(1)
cv.out = cv.glmnet(x[train, ], y[train], alpha = 0)
plot(cv.out, type = "o")
bestlambda = cv.out$lambda.min # 326
points(log(bestlambda), cv.out$cvm[which(cv.out$lambda == bestlambda)], col = "green", cex = 1)
ridge.pred = predict(ridge.mod, s = bestlambda, 
                     newx = x[test, ])
mean((ridge.pred - y.test) ^ 2) # 139857

# refit the ridge regression model on the full dataset, using the lambda chosen
# by the CV approach
out = glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlambda)[1:20, ]
# (Intercept)       AtBat        Hits       HmRun        Runs         RBI       Walks 
#     15.44383     0.07716     0.85912     0.60103     1.06369     0.87936     1.62445 
# Years      CAtBat       CHits      CHmRun       CRuns        CRBI      CWalks 
# 1.35255     0.01135     0.05747     0.40680     0.11456     0.12117     0.05299 
# LeagueN   DivisionW     PutOuts     Assists      Errors  NewLeagueN 
# 22.09143   -79.04033     0.16620     0.02942    -1.36093     9.12488

# The Lasso
lasso.mod = glmnet(x[train, ], y[train], alpha = 1, # if between 0 and 1, then is elastic net regularization
                   lambda = grid)
plot(lasso.mod)
set.seed(1)
cv.out = cv.glmnet(x[train, ], y[train], alpha = 1)
plot(cv.out)
bestlambda = cv.out$lambda.min
points(log(bestlambda), cv.out$cvm[which(cv.out$lambda == bestlambda)], col = "green")
lasso.pred = predict(lasso.mod, s = bestlambda, 
                     newx = x[test, ])
mean((lasso.pred - y.test) ^ 2) # 143674

# advantage of lasso over ridge regression: 
# higher interpretability since many coefficient estimates are 0
out = glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef = predict(out, type = "coefficients", s = bestlambda)[1:20, ]
lasso.coef
# (Intercept)       AtBat        Hits       HmRun        Runs         RBI       Walks 
#     1.27479    -0.05497     2.18035     0.00000     0.00000     0.00000     2.29192 
#   Years      CAtBat       CHits      CHmRun       CRuns        CRBI      CWalks 
# -0.33806     0.00000     0.00000     0.02825     0.21628     0.41713     0.00000 
#   LeagueN   DivisionW     PutOuts     Assists      Errors  NewLeagueN 
# 20.28615  -116.16756     0.23752     0.00000    -0.85629     0.00000

# The PCR and PLS Regression
# Principal Components Regression
library(pls)
set.seed(2)
pcr.fit = pcr(Salary ~ ., data = Hitters, 
              scale = TRUE, # standardizing each predictor
              validation = "CV") # compute the 10-fold CV errors for each possible value of M
summary(pcr.fit)
# Data: 	X dimension: 263 19 
# Y dimension: 263 1
# Fit method: svdpc
# Number of components considered: 19
# 
# VALIDATION: RMSEP
# Cross-validated using 10 random segments.
#         (Intercept)  1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps
# CV             452    351.9    353.2    355.0    352.8    348.4    343.6    345.5
# adjCV          452    351.6    352.7    354.4    352.1    347.6    342.7    344.7
#         8 comps  9 comps  10 comps  11 comps  12 comps  13 comps  14 comps  15 comps
# CV       347.7    349.6     351.4     352.1     353.5     358.2     349.7     349.4
# adjCV    346.7    348.5     350.1     350.7     352.0     356.5     348.0     347.7
#         16 comps  17 comps  18 comps  19 comps
# CV        339.9     341.6     339.2     339.6
# adjCV     338.2     339.7     337.2     337.6
# 
# TRAINING: % variance explained
#         1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps  8 comps
# X         38.31    60.16    70.84    79.03    84.29    88.63    92.26    94.96
# Salary    40.63    41.58    42.17    43.22    44.90    46.48    46.69    46.75
#         9 comps  10 comps  11 comps  12 comps  13 comps  14 comps  15 comps  16 comps
# X         96.28     97.26     97.98     98.65     99.15     99.47     99.75     99.89
# Salary    46.86     47.76     47.82     47.85     48.10     50.40     50.55     53.01
#         17 comps  18 comps  19 comps
# X          99.97     99.99    100.00
# Salary     53.85     54.61     54.61
# note that pcr() reports the root MSE

validationplot(pcr.fit, val.type = "MSEP") # to plot the MSE; "RMSEP" plots the root MSE

set.seed(1)
pcr.fit = pcr(Salary ~ ., data = Hitters, subset = train, 
              scale = TRUE, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP") # M = 5 yields the smallest MSE
pcr.fit = pcr(y ~ x, scale = TRUE, ncom = 5)
summary(pcr.fit)
# Data: 	X dimension: 263 19 
# Y dimension: 263 1
# Fit method: svdpc
# Number of components considered: 5
# TRAINING: % variance explained
#     1 comps  2 comps  3 comps  4 comps  5 comps
# X    38.31    60.16    70.84    79.03    84.29
# y    40.63    41.58    42.17    43.22    44.90

# Partial Least Squares
set.seed(1)
pls.fit = plsr(Salary ~ ., data = Hitters, subset = train, 
               scale = TRUE, validation = "CV")
summary(pls.fit)
# Data: 	X dimension: 131 19 
# Y dimension: 131 1
# Fit method: kernelpls
# Number of components considered: 19
# 
# VALIDATION: RMSEP
# Cross-validated using 10 random segments.
#         (Intercept)  1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps
# CV           428.3    325.5    329.9    328.8    339.0    338.9    340.1    339.0
# adjCV        428.3    325.0    328.2    327.2    336.6    336.1    336.6    336.2
#         8 comps  9 comps  10 comps  11 comps  12 comps  13 comps  14 comps  15 comps
# CV       347.1    346.4     343.4     341.5     345.4     356.4     348.4     349.1
# adjCV    343.4    342.8     340.2     338.3     341.8     351.1     344.2     345.0
#         16 comps  17 comps  18 comps  19 comps
# CV        350.0     344.2     344.5     345.0
# adjCV     345.9     340.4     340.6     341.1
# 
# TRAINING: % variance explained
#         1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps  8 comps
# X         39.13    48.80    60.09    75.07    78.58    81.12    88.21    90.71
# Salary    46.36    50.72    52.23    53.03    54.07    54.77    55.05    55.66
#         9 comps  10 comps  11 comps  12 comps  13 comps  14 comps  15 comps  16 comps
# X         93.17     96.05     97.08     97.61     97.97     98.70     99.12     99.61
# Salary    55.95     56.12     56.47     56.68     57.37     57.76     58.08     58.17
#         17 comps  18 comps  19 comps
# X          99.70     99.95    100.00
# Salary     58.49     58.56     58.62
validationplot(pls.fit, val.type = "MSEP")

pls.pred = predict(pls.fit, x[test, ], ncomp = 1)
mean((pls.pred - y.test) ^ 2) # 151995, higher than ridge and lasso regression, and PCR

# perform PLS using the full dataset, using M = 1
pls.fit = plsr(Salary ~ ., data = Hitters, scale = TRUE, ncomp = 1)
summary(pls.fit)
# Data: 	X dimension: 263 19 
# Y dimension: 263 1
# Fit method: kernelpls
# Number of components considered: 1
# TRAINING: % variance explained
#         1 comps
# X         38.08
# Salary    43.05