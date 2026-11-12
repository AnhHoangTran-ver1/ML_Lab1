# Assignment 3: Logistic regression and basis function expansion
# Part 1
library(ggplot2)

df <- read.csv("pima-indians-diabetes.csv", header = FALSE)
names(df) <- c("pregnant", "glucose", "bp", "triceps",
               "insulin", "bmi", "pedigree", "age", "diabetes")
df$diabetes <- factor(df$diabetes, levels = c(0, 1),
                      labels = c("No", "Yes"))

# Scatterplot: glucose on age, colored by diabetes
ggplot(df, aes(x = age, y = glucose, color = diabetes)) +
  geom_point(alpha = 0.7) +
  labs(x = "Age (years)", y = "Plasma glucose concentration",
       color = "Diabetes") +
  theme_minimal()

# Diabetes does not seem easy to classify with a standard logistic regression
# using only glucose and age. The classes overlap heavily: "Yes" and "No"
# points are mixed across the same age and glucose ranges. Higher glucose
# gives more "Yes" cases, but there is no clear separation, and a logistic
# regression can only produce a linear boundary.

# Part 2
# Fit the model
fit <- glm(diabetes ~ glucose + age, data = df, family = binomial)

r <- 0.5 # Predictions with threshold r = 0.5
prob <- predict(fit, type = "response")
df$pred <- factor(ifelse(prob > r, "Yes", "No"), levels = c("No", "Yes"))

# Training misclassification error
conf <- table(Predicted = df$pred, Actual = df$diabetes)
conf
miscl_error <- mean(df$pred != df$diabetes)
miscl_error

# Report the probabilistic equation of the estimated model
w <- coef(fit)
w0 <- w["(Intercept)"]
w1 <- w["glucose"]
w2 <- w["age"]

cat(sprintf(
  "p(y = 1 | x1, x2) = 1 / (1 + exp(-(%.4f + %.4f*x1 + %.4f*x2)))\n",
  w0, w1, w2))
cat("where x1 = plasma glucose, x2 = age, and y ~ Bernoulli(p)\n")
cat("Predict Diabetes = 1 if p > 0.5, i.e. if",
    sprintf("%.4f + %.4f*x1 + %.4f*x2 > 0\n", w0, w1, w2))

# Scatterplot with predicted classes as color
ggplot(df, aes(x = age, y = glucose, color = pred)) +
  geom_point(alpha = 0.7) +
  labs(x = "Age (years)", y = "Plasma glucose concentration",
       color = "Predicted diabetes") +
  theme_minimal()

# Comments regarding the results 
# The training misclassification error is 0.263, so about 26.3% of the observation 
# are classified incorrectly 
# Both glucose and age have positive coefficients so higher glucose and higher age 
# increase the predicted probability of diabetes. Glucose has stronger effect.
# The confusion matrix shows 138 true "Yes" cases predicted as "No" and 64 true "No"
# cases predicted as as "Yes". Thus it is more common to mistake "Yes" to "No" than t
# the opposite.
# In the predicted values plot the colors are separated by a straight line, while 
# the true classes in the first plot overlap heavily. A linear decision boundary 
# based on only glucose and age cannot separate the classes well, so the quality 
# of the classification is only moderate. This is why a more flexible model like
# basic function expansion is motivated. 

# Part 3
# a) Equation of the decision boundary
cat(sprintf("Decision boundary: glucose = %.4f + %.4f * age\n",
            -w0 / w1, -w2 / w1))

# b) Add the boundary to the scatterplot from step 2
ggplot(df, aes(x = age, y = glucose, color = pred)) +
  geom_point(alpha = 0.7) +
  geom_abline(intercept = unname(-w0 / w1), slope = unname(-w2 / w1),
              color = "black", linewidth = 1) +
  labs(x = "Age (years)", y = "Plasma glucose concentration",
       color = "Predicted diabetes") +
  theme_minimal()

# Here we can see that the boundary is a straight line and the true classes 
# overlap on both sides of it. So it does not seem to catch the data distribution
# well since there are many misclassed points 

# Part 4 
plot_threshold <- function(r) {
  pred_r <- factor(ifelse(prob > r, "Yes", "No"), levels = c("No", "Yes"))
  cat("Threshold r =", r, "\n")
  print(table(Predicted = pred_r, Actual = df$diabetes))
  cat("Misclassification error:", mean(pred_r != df$diabetes), "\n\n")
  
  ggplot(df, aes(x = age, y = glucose, color = pred_r)) +
    geom_point(alpha = 0.7) +
    geom_abline(intercept = unname((log(r / (1 - r)) - w0) / w1),
                slope = unname(-w2 / w1), color = "black", linewidth = 1) +
    labs(title = paste("Threshold r =", r),
         x = "Age (years)", y = "Plasma glucose concentration",
         color = "Predicted diabetes") +
    theme_minimal()
}

# plot_threshold(0.2)
# plot_threshold(0.8)

# The boundary line keeps the same slope and only shifts. For a smaller r, the 
# model predicts "yes" more easily so more points are predicted as "yes". On the 
# other hand, when r gets bigger, very few "yes" predictions so many true "yes" 
# are missed



# Part 5
add_features <- function(d) {
  d$z1 <- d$glucose^4
  d$z2 <- d$glucose^3 * d$age
  d$z3 <- d$glucose^2 * d$age^2
  d$z4 <- d$glucose   * d$age^3
  d$z5 <- d$age^4
  d
}

df5 <- add_features(df)

fit5 <- glm(diabetes ~ glucose + age + z1 + z2 + z3 + z4 + z5,
            data = df5, family = binomial)

prob5 <- predict(fit5, type = "response")
df5$pred5 <- factor(ifelse(prob5 > 0.5, "Yes", "No"), levels = c("No", "Yes"))

conf5 <- table(Predicted = df5$pred5, Actual = df5$diabetes)
miscl_error5 <- mean(df5$pred5 != df5$diabetes)
miscl_error5

# Scatterplot with the curved decision boundary p = 0.5
grid <- expand.grid(
  age     = seq(min(df5$age), max(df5$age), length.out = 200),
  glucose = seq(min(df5$glucose), max(df5$glucose), length.out = 200))
grid <- add_features(grid)
grid$prob5 <- predict(fit5, newdata = grid, type = "response")

ggplot(df5, aes(x = age, y = glucose, color = pred5)) +
  geom_point(alpha = 0.7) +
  geom_contour(data = grid, aes(x = age, y = glucose, z = prob5),
               breaks = 0.5, color = "black", inherit.aes = FALSE) +
  labs(x = "Age (years)", y = "Plasma glucose concentration",
       color = "Predicted diabetes") +
  theme_minimal()

# The training misclassification error is 0.245 for the model with the basis
# expansion, compared with 0.263 for the linear model. So the accuracy improved
# slightly (14 fewer misclassified observations out of 768). The model finds
# more of the true "Yes" cases (147 instead of 130 correct), while the number
# of false "Yes" predictions stays about the same (67 vs 64).
# The decision boundary is no longer a straight line. It is curved and
# non-monotonic in age: young people need a high glucose value to be predicted
# "Yes", the threshold is lowest around age 45-50, and then it rises steeply
# for older ages. This lets the model follow the data distribution more closely.
# However, the improvement is small and the classes still overlap heavily, so
# many points remain misclassified. The flexible boundary also creates a
# spurious "Yes" region in the lower right, where there is almost no data,
# which indicates overfitting risk. Since this is a training error, the
# improvement may partly be overfitting, so the extra complexity is not clearly
# worth it.




