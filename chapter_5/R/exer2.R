# Create a plot that displays, for each integer value of n from 1 to 100, 000, 
# the probability that the jth observation is in the bootstrap sample. 
# Comment on what you observe.

p_not_in_bootstrap_sample = function(n) {
  return ((1 - 1 / n) ^ n)
}

p_not_in_bootstrap_sample(10000)

p = rep(0, 100000)
for (i in seq(1, 100000)) {
  p[i] = 1 - p_not_in_bootstrap_sample(i)
}

plot(x = seq(1, 100000), 
     y = p, 
     xlab = "number of observations", 
     ylab = "p of the j-th observation in bootstrap sample", 
     log = "x", 
     type = "o")

store = rep(NA, 10000)
for (i in 1:10000) {
  store[i] = sum(sample(1:100, rep = T) == 4) > 0
}
mean(store)

1 - p_not_in_bootstrap_sample(100)
