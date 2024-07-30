# Load necessary libraries
library(MASS)
library(glmnet)
library(ggplot2)

# Set seed for reproducibility
set.seed(2024)

# Simulation parameters
N <- 100
sigma_sq <- 2
correlation <- 0.2
beta_mean <- 0
beta_sd <- 1

# Number of features
p_values <- c(20, 100, 1000)

# Regularization parameters
lambdas <- c(0.001, 100, 1000)

# Function to generate correlated Gaussian features
generate_features <- function(N, p, correlation) {
  Sigma <- matrix(correlation, nrow = p, ncol = p)
  diag(Sigma) <- 1
  mvrnorm(N, mu = rep(0, p), Sigma = Sigma)
}

# Function to generate outcome
generate_outcome <- function(X, beta, sigma) {
  epsilon <- rnorm(nrow(X), mean = 0, sd = sigma)
  Y <- X %*% beta + epsilon
  Y
}

# Initialize lists to store results
test_errors <- list()
effective_dof <- list()

for (p in p_values) {
  for (i in 1:100) {
    # Generate coefficients
    beta <- rnorm(p, mean = beta_mean, sd = beta_sd)
    
    # Generate features
    X <- generate_features(N, p, correlation)
    
    # Generate outcome
    sigma <- sqrt(var(X %*% beta) / sigma_sq)
    Y <- generate_outcome(X, beta, sigma)
    
    # Split data into training and test sets
    train_idx <- sample(1:N, size = 0.9 * N)
    test_idx <- setdiff(1:N, train_idx)
    
    X_train <- X[train_idx, ]
    Y_train <- Y[train_idx]
    X_test <- X[test_idx, ]
    Y_test <- Y[test_idx]
    
    # Fit ridge regression models and calculate test errors
    for (lambda in lambdas) {
      fit <- glmnet(X_train, Y_train, alpha = 0, lambda = lambda)
      predictions <- predict(fit, X_test)
      test_error <- mean((Y_test - predictions)^2)
      relative_error <- test_error / sigma_sq
      test_errors[[paste(p, lambda, sep = "_")]] <- c(test_errors[[paste(p, lambda, sep = "_")]], relative_error)
      
      # Calculate effective degrees of freedom using formula 
      d <- svd(X_train)$d
      df <- sum(d^2 / (d^2 + lambda))
      effective_dof[[paste(p, lambda, sep = "_")]] <- c(effective_dof[[paste(p, lambda, sep = "_")]], df)
    }
  }
}
