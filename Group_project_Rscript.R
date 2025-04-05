# Fraud Detection Group Project

# Import libraries
library(tidyverse)

# Load data
df <- read.csv("C:/Users/veron/OneDrive - Kormányzati Informatikai Fejlesztési Ügynökség/Dokumentumok/fraud_dataset.csv",
               stringsAsFactors = FALSE)

# Exploration
str(df)
summary(df)
table(df$isFraud)

# Check for missing values
colSums(is.na(df))

# Convert variables
df$Transaction_Type <- as.factor(df$Transaction_Type)
df$Fraudulent <- as.factor(df$Fraudulent)

# Exploration with Visuals1: Distribution of transaction types
ggplot(df, aes(x = Transaction_Type, fill = Fraudulent)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Fraud by Transaction Type",
       x = "Transaction Type", y = "Proportion", fill = "Fraudulent") +
  theme_minimal()

# Exploration with Visuals2: Distribution of amounts
ggplot(df, aes(x = Transaction_Amount, fill = Fraudulent)) +
  geom_density(alpha = 0.6) +
  scale_x_log10() +
  labs(title = "Transaction Amount Distribution by Fraud Class",
       x = "Transaction Amount (log scale)", y = "Density", fill = "Fraudulent") +
  theme_minimal()

# Exploration with Visuals3: Device used and Fraudulent transaction relationship plot
df$Device_Used <- as.factor(df$Device_Used)
ggplot(df, aes(x = Device_Used, fill = Fraudulent)) +
  geom_bar(position = "dodge") +
  labs(title = "Fraud by Device Type",
       x = "Device Used", y = "Count", fill = "Fraudulent") +
  theme_minimal()

# Exploration with Visuals4: Location based distribution plot
# Get top 10 locations with most frauds
top_locs <- df %>%
  filter(Fraudulent == 1) %>%
  count(Location, sort = TRUE) %>%
  slice_max(n, n = 10) %>%
  pull(Location)

# Filter and plot
ggplot(df %>% filter(Location %in% top_locs),
       aes(x = Location, fill = Fraudulent)) +
  geom_bar(position = "dodge") +
  labs(title = "Fraud by Location (Top 10)",
       x = "Location", y = "Count", fill = "Fraudulent") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Exploration with Visuals5: Fraudulent transactions amount distribution plot
# Filter fraud-only and plot amount distribution
ggplot(df %>% filter(Fraudulent == 1),
       aes(x = Transaction_Amount)) +
  geom_histogram(bins = 30, fill = "red", alpha = 0.7) +
  scale_x_log10() +
  labs(title = "Transaction Amounts - Fraudulent Only",
       x = "Transaction Amount (log scale)", y = "Count") +
  theme_minimal()

# Train/Test split
install.packages("caret")
library(caret)

set.seed(123)
train_index <- caret::createDataPartition(df$Fraudulent, p = 0.7, list = FALSE)
train <- df[train_index, ]
test <- df[-train_index, ]

# Logistic Regression

# Random Forest

# Evaluation

# Evaluate Logistic regression

# Evaluate Random Forest

# Variable Importance Plot

