install.packages("caret")
install.packages("tidyverse")
install.packages("mlbench")
library("caret")
library("tidyverse")
library("mlbench")

# split data 70:30
set.seed(10)
n <- nrow(mtcars)
id <- sample(1:n, size = 0.7 * n)
train_df <- mtcars[id, ]
test_df <- mtcars[-id, ]

# train
set.seed(10)
lm_model <- train(mpg ~ hp + wt + am,
                  data = train_df,
                  method = "lm")

# score
p_test <- predict(lm_model,
                  newdata = test_df)

# evaluate MAE, MSE, RMSE
error <- test_df$mpg - p_test
mae <- mean(abs(error))
mse <- mean(error**2)
rmse <- sqrt(mean(error**2))

# knn
# train
set.seed(10)
knn_model <- train(mpg ~ hp + wt + am,
                  data = train_df,
                  method = "knn")

# score
p_test_knn <- predict(knn_model,
                  newdata = test_df)

# evaluate MAE, MSE, RMSE
error_knn <- test_df$mpg - p_test_knn
rmse <- sqrt(mean(error_knn**2))
