library(ggplot2)
library(caret)


data = read.csv("/Users/tatevhakobyan/Desktop/semester1/ML/labs/ML_Lab1/parkinsons.csv", header = TRUE)
head(data)
summary(data)
colnames(data)

# Part 1
# Division of the data into training and testing data sets and scaling it
set.seed(12345)
n=nrow(data)
id=sample(1:n, floor(n*0.6))
train=data[id,]
test=data[-id,]


# Part 2
# Computing the linear regression model

scaler=preProcess(train)
trainS=predict(scaler,train)
testS=predict(scaler,test)


# Part 2
# Computing the linear regression model


drop_cols <- c("subject.","age","sex","test_time","total_UPDRS")
trainS <- trainS[, !(names(trainS) %in% drop_cols)]
testS  <- testS[,  !(names(testS)  %in% drop_cols)]


# linear model
fit <- lm(motor_UPDRS ~ 0 + ., data = trainS)
summary(fit)

# DFA, PPE, HNR, Shimmer.APQ11, Jitter.Abs., NHR, Shimmer.APQ5 and Shimmer contribute significantly to the model (p < 0.05), 
# DFA, PPE and HNR being the most significant (p < 1e-10).


# making predictions on the training set

train_predictions <- predict(fit, newdata = trainS)

# Calculate the Training MSE
train_actuals <- trainS$motor_UPDRS
train_mse <- mean((train_actuals - train_predictions)^2)

# Print the Training MSE
print(paste("Training MSE:", train_mse))

# Make predictions on the test set
test_predictions <- predict(fit, newdata = testS)

# Calculate the Test MSE
test_actuals <- testS$motor_UPDRS
test_mse <- mean((test_actuals - test_predictions)^2)

# Print the Test MSE
print(paste("Test MSE:", test_mse))


# Part 3

# loglikelihood

Loglikelihood <- function(theta, sigma) {
  X <- as.matrix(trainS[, colnames(trainS) != "motor_UPDRS"])
  y <- trainS$motor_UPDRS
  n <- nrow(X)
  p <- ncol(X)
  res <- y - X %*% theta
  return(-n/2 * log(2 * pi * sigma^2) - sum(res^2) / (2 * sigma^2))
}

Ridge <- function(theta, sigma, lambda) {
  -Loglikelihood(theta, sigma) + lambda * sum(theta^2)
}

RidgeOpt <- function(lambda) {
  obj <- function(par) Ridge(par[1:p], par[p + 1], lambda)
  start <- c(rep(0, p), 1)
  opt <- optim(par = start, fn = obj, method = "BFGS")
  list(theta = opt$par[1:p],
       sigma = opt$par[p + 1],
       value = opt$value,
       convergence = opt$convergence)
}

# degrees of freedom

DF <- function(lambda) {
  H <- X %*% solve(t(X) %*% X + lambda * diag(p)) %*% t(X)
  sum(diag(H))
}

# Part 4
# Ridge regression

X <- as.matrix(trainS[, colnames(trainS) != "motor_UPDRS"])
y <- trainS[, "motor_UPDRS"]
n <- nrow(X)
p <- ncol(X)

# test data in the same form as training data
Xtest <- as.matrix(testS[, colnames(testS) != "motor_UPDRS"])
ytest <- testS[, "motor_UPDRS"]

lambdas <- c(1, 100, 1000)

results <- data.frame(lambda = lambdas,
                      train_MSE = NA, test_MSE = NA, DF = NA, sigma = NA)

thetas <- list()

for (i in seq_along(lambdas)) {
  fit <- RidgeOpt(lambdas[i])
  theta <- fit$theta
  thetas[[i]] <- theta
  
  pred_train <- X %*% theta
  pred_test  <- Xtest %*% theta
  
  results$train_MSE[i] <- mean((y - pred_train)^2)
  results$test_MSE[i]  <- mean((ytest - pred_test)^2)
  results$DF[i]        <- DF(lambdas[i])
  results$sigma[i]     <- fit$sigma
}

print(results)


