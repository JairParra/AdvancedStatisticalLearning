# Load required packages
library(mvtnorm)
library(glmnet)
library(caret)

# Set seed for reproducibility
set.seed(123)

# Generate bivariate normal data
n <- 1000 # Number of samples
mu <- c(0, 0) # Mean vector
sigma <- matrix(c(1, 0.5, 0.5, 1), nrow = 2) # Covariance matrix
data <- rmvnorm(n,
    mean = mu,
    sigma = sigma
)

# Split data into train, validation, and test sets
train_ratio <- 0.7
val_ratio <- 0.15
test_ratio <- 0.15

train_size <- round(train_ratio * n)
val_size <- round(val_ratio * n)
test_size <- n - train_size - val_size

train_data <- data[1:train_size, ]
val_data <- data[(train_size + 1):(train_size + val_size), ]
test_data <- data[(train_size + val_size + 1):n, ]

# Fit elasticnet regression model on train set
model <- cv.glmnet(
    x = train_data[, -2],
    y = train_data[, 2],
    alpha = 0.5
)

# Select best hyperparameter lambda using validation set
best_lambda <- model$lambda.min

# Refit model on train and validation sets combined
combined_data <- rbind(train_data, val_data)
final_model <- glmnet(
    x = combined_data[, -2],
    y = combined_data[, 2], 
    alpha = 0.5, 
    lambda = best_lambda
) # nolint: line_length_linter.

# Predict on test set
test_predictions <- predict(final_model, newx = test_data[, -2])