# Fraud Detection Group Project

# Import libraries
library(tidyverse)


# Load data
df <- read.csv("fraud_dataset.csv")

# Exploration
str(df)
summary(df)
table(df$isFraud)

# Check for missing values
colSums(is.na(df))

# Convert variables
df$type <- as.factor(df$type)
df$isFraud <- as.factor(df$isFraud)

# Train/Test split
set.seed(123)
index <- createDataPartition(df$isFraud, p = 0.7, list = FALSE)
train <- df[index, ]
test <- df[-index, ]


#Logistic Regression
log_model <- glm(isFraud ~ ., data = train_bal, family = "binomial")
log_pred <- predict(log_model, test, type = "response")
log_pred_class <- ifelse(log_pred > 0.5, 1, 0)

# Random Forest
rf_model <- randomForest(isFraud ~ ., data = train_bal, ntree = 100, importance = TRUE)
rf_pred <- predict(rf_model, test)

# Evaluation
evaluate_model <- function(actual, predicted) {
  cm <- confusionMatrix(as.factor(predicted), as.factor(actual), positive = "1")
  print(cm)
  roc_obj <- roc(as.numeric(actual), as.numeric(predicted))
  print(paste("AUC:", auc(roc_obj)))
  plot(roc_obj, main = "ROC Curve")
}

# Evaluate Logistic regression
cat("Logistic Regression Performance:\n")
evaluate_model(test$isFraud, log_pred_class)

# Evaluate Random Forest
cat("Random Forest Performance:\n")
evaluate_model(test$isFraud, rf_pred)

# Variable Importance Plot
varImpPlot(rf_model)

