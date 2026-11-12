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

# Part 2
# Fit the model
fit <- glm(diabetes ~ glucose + age, data = df, family = binomial)
summary(fit)

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

# The boundary line keeps the same slope and only shifts. For a smaller r, the 
# model predicts "yes" more easily so more points are predicted as "yes". On the 
# other hand, when r gets bigger, very few "yes" predictions so many true "yes" 
# are missed







