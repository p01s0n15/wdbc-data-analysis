if (!requireNamespace("readr", quietly = TRUE)) {
  install.packages("readr")
}
if (!requireNamespace("glmnet", quietly = TRUE)) {
  install.packages("glmnet")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

library(readr)
library(glmnet)
library(ggplot2)

load_and_preprocess_data <- function(file_path) {
  data <- read.csv(file_path, header = FALSE)
  colnames(data) <- c("ID", "Diagnosis", paste0("Feature", 1:30))
  data$Diagnosis <- as.integer(data$Diagnosis == "M")
  return(data)
}
split_data <- function(data, split_ratio = 0.8) {
  set.seed(1234)
  train_index <- sample(1:nrow(data), split_ratio * nrow(data))
  train <- data[train_index, ]
  test <- data[-train_index, ]
  return(list(train = train, test = test))
}
build_linear_model <- function(train_data) {
  model <- lm(Diagnosis ~ ., data = train_data)
  return(model)
}
evaluate_model <- function(model, test_data) {
  predictions <- predict(model, newdata = test_data)
  binaryPredictions <- as.integer(predictions > 0.5)
  accuracy <- mean(binaryPredictions == test_data$Diagnosis)
  return(accuracy)
}
create_histogram_plot <- function(data, column) {
  ggplot(data, aes(x = data[[column]])) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
    labs(title = paste("Histogram of", column), x = column, y = "Frequency")
}
create_density_plot <- function(data, column) {
  ggplot(data, aes(x = data[[column]])) +
    geom_density(fill = "blue", color = "black", alpha = 0.7) +
    labs(title = paste("Density Plot of", column), x = column, y = "Density")
}
create_violin_plot <- function(data, column) {
  ggplot(data, aes(x = as.factor(data$Diagnosis), y = data[[column]])) +
    geom_violin(fill = "blue", color = "black", alpha = 0.7) +
    labs(title = paste("Violin Plot of", column, "by Diagnosis"), x = "Diagnosis", y = column) +
    theme_minimal()
}
# Main code
file_path <- "///***enter file path here***///"
wdbc_data <- load_and_preprocess_data(file_path)

data_splits <- split_data(wdbc_data)
linear_regression_model <- build_linear_model(data_splits$train)
accuracy <- evaluate_model(linear_regression_model, data_splits$test)
print(paste("Linear Regression Model Accuracy:", accuracy))

create_histogram_plot(wdbc_data, "Feature4")
create_density_plot(wdbc_data, "Feature4")
create_violin_plot(wdbc_data, "Feature4")
