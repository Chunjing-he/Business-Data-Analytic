# Load necessary libraries
library(ggplot2)
library(corrplot)
library(readr)
library(dplyr)
library(psych)

# Read the data
data <- read.csv("Downloads/Data1(1) (1).csv")
Data1_1_1_ <- read_csv("Downloads/Data1(1) (1).csv")

# Data cleaning
# Check for missing values
missing_values <- sum(is.na(data))
if (missing_values > 0) {
  # Here we simply remove rows with missing values. Other methods like mean/median imputation can also be used.
  data <- data %>% drop_na()
}

# Check for outliers. Take the 'difficulty' variable as an example and use a boxplot to visualize.
ggplot(data, aes(x = factor(1), y = difficulty)) +
  geom_boxplot()
# Based on the boxplot results, if there are outliers, corresponding processing can be done. Here, we assume there are no outliers and do not perform any processing.

# Select independent and dependent variables
# Assume the dependent variable is Q10 (The degree to which the initial expectations for the course were met at the end of the semester or academic year).
# Select some independent variables, such as the relevance of the teacher's knowledge (Q13), the adequacy of the teacher's preparation (Q14), etc.
dependent_variable <- data$Q10
independent_variables <- data %>% select(Q13, Q14, Q15, Q16, Q17, Q18, Q19, Q20, Q21, Q22, Q23, Q24, Q25, Q26, Q27, Q28)

# Dimensionality reduction using Principal Component Analysis (PCA)
# Check if the data is suitable for PCA by calculating the KMO and Bartlett tests
KMO(independent_variables)
cortest.bartlett(independent_variables)
# Based on the test results, if suitable for PCA, perform dimensionality reduction
pca_result <- prcomp(independent_variables, scale = TRUE)
# View the PCA results
summary(pca_result)
# Select appropriate principal components. Here, we select the first two principal components based on the cumulative contribution rate.
num_pc <- 2 
selected_pc <- pca_result$x[, 1:num_pc]
# Combine the principal components with the dependent variable
new_data <- data.frame(dependent_variable, selected_pc)

# Descriptive statistical analysis
# Perform descriptive statistics on the dependent variable
dependent_summary <- summary(dependent_variable)
print(dependent_summary)
# Perform descriptive statistics on the independent variables (principal components)
independent_summary <- summary(selected_pc)
print(independent_summary)

# Correlation analysis
correlation_matrix <- cor(new_data)
print(correlation_matrix)
# Visualize the correlation using a heatmap
library(corrplot)
corrplot(correlation_matrix, method = "color")

# Visualization analysis
# Plot the distribution of the dependent variable
ggplot(data, aes(x = Q10)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Distribution of Q10", x = "Q10", y = "Frequency")
# Plot scatter plots of the independent variables (principal components) against the dependent variable
for (i in 1:num_pc) {
  ggplot(new_data, aes(x = selected_pc[, i], y = dependent_variable)) +
    geom_point() +
    labs(title = paste("Scatter Plot of PC", i, "vs Q10"), x = paste("PC", i), y = "Q10")
}

# 获取当前用户的下载文件夹路径
download_path <- file.path(Sys.getenv("HOME"), "Downloads")

# 构建完整的文件路径
file_path <- file.path(download_path, "exploratory.csv")

# 将包含新变量的数据保存为 exploratory.csv
write.csv(data, file = file_path, row.names = FALSE)
