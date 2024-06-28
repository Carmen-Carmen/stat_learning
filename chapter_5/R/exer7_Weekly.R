library(ISLR2)

attach(Weekly)
summary(Weekly)

# (a)
glm.fit = glm(Direction ~ Lag1 + Lag2, 
              data = Weekly, 
              family = binomial)
summary(glm.fit)

# (b)
glm.fit1 = glm(Direction ~ Lag1 + Lag2, 
               data = Weekly[-1, ],
               family = binomial)

# (c)
predict(glm.fit1, Weekly[1, c("Lag1", "Lag2")], type = "response") > .5 # True
Weekly[1, "Direction"] # Down
# wrong prediction for the first observation

# (d)
glm.pred = rep("Down", nrow(Weekly))
for (i in 1:nrow(Weekly)) {
  fit = glm(Direction ~ Lag1 + Lag2, 
                data = Weekly[-i, ], 
                family = binomial)
  new_data = Weekly[i, c("Lag1", "Lag2")]
  prob = predict(fit, new_data, type = "response")
  if (prob > .5) {
    glm.pred[i] = "Up"
  }
}

mean(glm.pred != Direction) # 0.4499541
