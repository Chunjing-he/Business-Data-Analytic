# 加载必要的R库
library(dplyr)         # 数据处理
library(ggplot2)       # 可视化
library(caret)         # 机器学习模型
library(randomForest)  # 随机森林
library(e1071)         # SVM
library(caTools)       # 数据拆分
library(ROSE)          # 处理类别不均衡问题

# 读取数据
data <- read.csv("Downloads/Data2.csv")

# 查看数据基本信息
str(data)
summary(data)

# 处理缺失值（删除）
data <- na.omit(data)  # 直接删除缺失值

# 仅选择数据集中存在的分类变量
categorical_vars <- c("job", "marital", "education", "default", "housing", "loan", 
                      "contact", "month", "day_of_week", "poutcome", "y")

# 获取数据集中真实存在的分类变量
actual_categorical_vars <- intersect(categorical_vars, colnames(data))

# Print existing categorical variables
print("Existing categorical variables in the dataset:")
print(actual_categorical_vars)

# Convert only existing categorical variables to factors
for (var in actual_categorical_vars) {
  data[[var]] <- as.factor(data[[var]])
}

# Check and remove unnecessary variables (e.g., 'duration' if present)
if ("duration" %in% colnames(data)) {
  data <- select(data, -duration)
}

# Split data into training (80%) and testing (20%) sets using base R
set.seed(123)
train_indices <- sample(1:nrow(data), 0.8 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# ====================
# Train Classification Model: Logistic Regression
# ====================
logistic_model <- glm(y ~ ., data = train_data, family = binomial)

# Make predictions on the test set
logistic_pred <- predict(logistic_model, test_data, type = "response")
logistic_pred_class <- ifelse(logistic_pred > 0.5, "yes", "no")

# Compute accuracy
logistic_acc <- mean(logistic_pred_class == test_data$y)
print(paste("Logistic Regression Accuracy: ", round(logistic_acc, 4)))

# ====================
# Train Classification Model: Random Forest
# ====================
rf_model <- randomForest(y ~ ., data = train_data, ntree = 100)
rf_pred <- predict(rf_model, test_data)
rf_acc <- mean(rf_pred == test_data$y)
print(paste("Random Forest Accuracy: ", round(rf_acc, 4)))

# ====================
# Train Classification Model: Support Vector Machine (SVM)
# ====================
svm_model <- svm(y ~ ., data = train_data, kernel = "radial")  # Using radial kernel
svm_pred <- predict(svm_model, test_data)
svm_acc <- mean(svm_pred == test_data$y)
print(paste("SVM Accuracy: ", round(svm_acc, 4)))

# ====================
# Train Regression Model (Predicting balance)
# ====================

if ("balance" %in% colnames(data)) {
  regression_model <- lm(balance ~ age + campaign + pdays + previous, data = train_data)
  reg_pred <- predict(regression_model, test_data)
  reg_mse <- mean((reg_pred - test_data$balance)^2)
  print(paste("Linear Regression MSE: ", round(reg_mse, 4)))
  
  # Train Random Forest Regression Model
  rf_reg_model <- randomForest(balance ~ age + campaign + pdays + previous, data = train_data, ntree = 100)
  rf_reg_pred <- predict(rf_reg_model, test_data)
  rf_reg_mse <- mean((rf_reg_pred - test_data$balance)^2)
  print(paste("Random Forest Regression MSE: ", round(rf_reg_mse, 4)))
} else {
  print("⚠ Warning: 'balance' variable does not exist in the dataset. Skipping regression analysis.")
}

# ====================
# Model Performance Comparison
# ====================
model_performance <- data.frame(
  Model = c("Logistic Regression", "Random Forest (Class)", "SVM (Class)", "Linear Regression", "Random Forest (Reg)"),
  Accuracy_or_MSE = c(logistic_acc, rf_acc, svm_acc, ifelse(exists("reg_mse"), reg_mse, NA), 
                      ifelse(exists("rf_reg_mse"), rf_reg_mse, NA))
)

# Print model performance
print("Model performance comparison:")
print(model_performance)


# 获取当前用户的下载文件夹路径
download_path <- file.path(Sys.getenv("HOME"), "Downloads")

# 构建完整的文件路径
file_path <- file.path(download_path, "analysis.csv")

# 将包含新变量的数据保存为 analysis.csv
write.csv(data, file = file_path, row.names = FALSE)
