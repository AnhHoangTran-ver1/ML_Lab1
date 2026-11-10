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

# Scatterplot with predicted classes as color
ggplot(df, aes(x = age, y = glucose, color = pred)) +
  geom_point(alpha = 0.7) +
  labs(x = "Age (years)", y = "Plasma glucose concentration",
       color = "Predicted diabetes") +
  theme_minimal()

# Comments regarding the results 


# Assignment 4: Theory
# Part 2

