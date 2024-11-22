# model for predict admission test score by age and gender
library(ggplot2)
library(lattice)
library(tidyverse)
library(caret)
data <- read.csv("student_admission_record_dirty.csv")
str(data)
summary(data)
head(data)
data$Gender <- factor(data$Gender, levels = c("Male", "Female"))
# clean data
data <- na.omit(data)
model <- lm(Admission.Test.Score ~ Age + Gender, data = data)
summary(model)
# split data 70:30
set.seed(42)
n <- nrow(data)
id <- sample(1:n, size=0.7*n)
train_df <- data[id, ]
test_df <- data[-id, ]
# k-fold cross validation
set.seed(42)
ctrl <- trainControl(method = "cv",
                     number= 5,
                     verboseIter = TRUE)
# train linear regression model
set.seed(42)
lm_model <- train(Admission.Test.Score ~ Age + Gender, 
                  data = train_df,
                  method = "lm",
                  trControl = ctrl,)
# train knn model
knn_model <- train(Admission.Test.Score ~ Age + Gender, 
                   data = train_df,
                   method = "knn",
                   trControl = ctrl)
# score
p_test <- predict(lm_model, newdata = test_df)
p_test_knn <- predict(knn_model, newdata = test_df)
# evaluate MAE, MSE, RMSE
error <- test_df$Admission.Test.Score - p_test
error_knn <- test_df$Admission.Test.Score - p_test_knn

mae <- mean(abs(error))
mae_knn <- mean(abs(error_knn))

mse <- mean(error**2)
rmse <- sqrt(mse)

