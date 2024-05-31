# Load the necessary packages
library(MASS)
library(plotly)
library(mvtnorm)

# Set the mean vector and covariance matrix for the bivariate normal distribution
mean_vector <- c(0, 0)
cov_matrix <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)

# Generate data using the mvrnorm function
set.seed(123)  # For reproducibility
data <- mvrnorm(n = 1000, mu = mean_vector, Sigma = cov_matrix)

# Create a grid of values
x <- seq(-3, 3, length.out = 100)
y <- seq(-3, 3, length.out = 100)
z <- outer(x, y, function(x, y) {
  dmvnorm(cbind(x, y), mean = mean_vector, sigma = cov_matrix)
})

# Convert the grid to a format suitable for plotly
z <- as.vector(z)
grid <- expand.grid(x = x, y = y)
grid$z <- z

# Create the 3-D plot
plot_ly(grid, x = ~x, y = ~y, z = ~z, type = "mesh3d") %>%
  layout(scene = list(
    xaxis = list(title = "X"),
    yaxis = list(title = "Y"),
    zaxis = list(title = "Density")
  ))
