# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readr)
library(corrplot)
library(caret)  # For model training and evaluation

dir <- getwd()
path <- paste(dir, "healthcare-dataset-stroke-data.csv", sep='/')
df <- read_csv(path)

# Overview of the dataset
str(df)
head(df)

# 1. Data Cleaning and Preparation

# Fill missing values in the 'bmi' column with the mean
df$bmi[is.na(df$bmi)] <- mean(df$bmi, na.rm = TRUE)

# Ensure 'bmi' is numeric
df$bmi <- as.numeric(df$bmi)

# Detect and handle outliers
bmi_threshold <- 50
df$bmi <- ifelse(df$bmi > bmi_threshold, mean(df$bmi, na.rm = TRUE), df$bmi)

glucose_threshold <- 200
df$avg_glucose_level <- ifelse(df$avg_glucose_level > glucose_threshold, mean(df$avg_glucose_level, na.rm = TRUE), df$avg_glucose_level)

# Convert categorical variables to factors
df$gender <- as.factor(df$gender)
df$hypertension <- as.factor(df$hypertension)
df$heart_disease <- as.factor(df$heart_disease)
df$ever_married <- as.factor(df$ever_married)
df$work_type <- as.factor(df$work_type)
df$Residence_type <- as.factor(df$Residence_type)
df$smoking_status <- as.factor(df$smoking_status)
df$stroke <- as.factor(df$stroke)

# 2. Splitting the Data into Training and Testing Sets
set.seed(123)  # For reproducibility
train_index <- createDataPartition(df$stroke, p = 0.7, list = FALSE)
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

# 3. Logistic Regression Model

# Fit the model
model <- glm(stroke ~ age + hypertension + heart_disease + avg_glucose_level + bmi + gender + ever_married + work_type + Residence_type + smoking_status,
             data = train_data, family = binomial)

# Summary of the model
summary(model)

# 4. Model Evaluation

# Predict on the test set
test_predictions <- predict(model, test_data, type = "response")
test_data$predicted_stroke <- ifelse(test_predictions > 0.5, 1, 0)


# Confusion Matrix
confusion_matrix <- table(test_data$stroke, test_data$predicted_stroke)
print("Confusion Matrix:")
print(confusion_matrix)

# Calculate Accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Model Accuracy: ", round(accuracy * 100, 2), "%\n")

# 5. ROC Curve and AUC
library(pROC)
roc_curve <- roc(test_data$stroke, test_predictions)
plot(roc_curve, main = "ROC Curve", col = "blue")
cat("AUC: ", auc(roc_curve), "\n")

