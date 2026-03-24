library(dplyr)
library(ggplot2)
library(readr)
library(corrplot)

dir <- getwd()
path <- paste(dir, "healthcare-dataset-stroke-data.csv",sep='/')
df <- read_csv(path)

str(df)
head(df)

# 1. Check for missing values
print("Total missing values in the dataset:")
sum(is.na(df))

# Check missing values per column
print("Missing values per column:")
colSums(is.na(df))

# Fill missing values in the 'bmi' column with the mean of 'bmi'
df$bmi[is.na(df$bmi)] <- mean(df$bmi, na.rm = TRUE)

# Convert bmi to numeric if necessary
df$bmi <- as.numeric(df$bmi)

# 2. Detect and replace outliers in numerical columns

# Define threshold for 'bmi' column and replace values above 50 with the mean
bmi_threshold <- 50
df$bmi <- ifelse(df$bmi > bmi_threshold, mean(df$bmi, na.rm = TRUE), df$bmi)

# Define threshold for 'avg_glucose_level' (e.g., above 200 is an outlier)
glucose_threshold <- 200
df$avg_glucose_level <- ifelse(df$avg_glucose_level > glucose_threshold, mean(df$avg_glucose_level, na.rm = TRUE), df$avg_glucose_level)

# 3. Data Visualization

# Gender distribution - bar plot
ggplot(df, aes(x = gender)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Gender Distribution", x = "Gender", y = "Count")

# Ever Married distribution - bar plot
ggplot(df, aes(x = ever_married)) + 
  geom_bar(fill = "blue") +
  labs(title = "Married Status Distribution", x = "Married" , y = "Count")

# Stroke outcome - pie chart
stroke_count <- table(df$stroke)
pie(stroke_count, labels = c("No Stroke", "Stroke"), main = "Stroke Outcome Distribution", col = c("lightgreen", "salmon"))

# Residence type distribution - bar plot
ggplot(df, aes(x = Residence_type)) +
  geom_bar(fill = "purple") +
  labs(title = "Residence Type Distribution", x = "Residence Type", y = "Count")

# Work type distribution - bar plot
ggplot(df, aes(x = work_type)) +
  geom_bar(fill = "darkorange") +
  labs(title = "Work Type Distribution", x = "Work Type", y = "Count")

# Smoking status distribution - bar plot
ggplot(df, aes(x = smoking_status)) +
  geom_bar(fill = "brown") +
  labs(title = "Smoking Status Distribution", x = "Smoking Status", y = "Count")

# Hypertension and Heart Disease - bar plot
ggplot(df, aes(x = as.factor(hypertension))) +
  geom_bar(fill = "skyblue") +
  labs(title = "Hypertension Distribution", x = "Hypertension", y = "Count") +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes"))

ggplot(df, aes(x = as.factor(heart_disease))) +
  geom_bar(fill = "pink") +
  labs(title = "Heart Disease Distribution", x = "Heart Disease", y = "Count") +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes"))

# 4. Numerical Data Analysis

# Age distribution - histogram
ggplot(df, aes(x = age)) +
  geom_histogram(bins = 30, fill = "blue", color = "white") +
  labs(title = "Age Distribution", x = "Age", y = "Count")

# BMI distribution - histogram
ggplot(df, aes(x = bmi)) +
  geom_histogram(bins = 30, fill = "green", color = "white") +
  labs(title = "BMI Distribution", x = "BMI", y = "Count")

# Average glucose level distribution - histogram
ggplot(df, aes(x = avg_glucose_level)) +
  geom_histogram(bins = 30, fill = "red", color = "white") +
  labs(title = "Average Glucose Level Distribution", x = "Avg Glucose Level", y = "Count")

# 5. Correlation Analysis for Numeric Columns

# Select numeric columns for correlation matrix
numeric_df <- df %>% select_if(is.numeric)

# Calculate correlation matrix
cor_matrix <- cor(numeric_df, use = "complete.obs")


# Plot correlation matrix
corrplot(cor_matrix, method = "circle")

# 6. Summary of Key Insights

# Gender distribution summary
gender_dist <- df %>% group_by(gender) %>% summarise(Count = n())
print(gender_dist)

# Stroke rate based on gender
stroke_by_gender <- df %>% group_by(gender) %>% summarise(Stroke_Rate = mean(stroke))
print(stroke_by_gender)

# Hypertension and heart disease prevalence
hypertension_rate <- mean(df$hypertension)
heart_disease_rate <- mean(df$heart_disease)
cat("Hypertension prevalence: ", hypertension_rate * 100, "%\n")
cat("Heart disease prevalence: ", heart_disease_rate * 100, "%\n")

# Smoking status breakdown
smoking_dist <- df %>% group_by(smoking_status) %>% summarise(Count = n())
print(smoking_dist)

# Residence type breakdown
residence_dist <- df %>% group_by(Residence_type) %>% summarise(Count = n())
print(residence_dist)

