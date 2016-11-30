# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co


# Dataset
head(iris)
dim(iris)  # 150 rows and 5 columns.

# Distance Matrix ---------------------------------------------------
d1 <- dist(iris[1:10, c(1:4)])   # distance matrix on original data 
iris[1:2, ]

# Normalize ---------------------------------------------------------
normalize <- function(x){
  return((x-min(x))/(max(x) - min(x)))
}

normalized_iris <- lapply(iris[, -5], normalize)
iris_n <- data.frame(normalized_iris, Species=iris[, 5])


d2 <- dist(iris_n[1:10, c(1:4)])  # distance matrix on normalized data
d2

# KNN Classification ------------------------------------------------
# Create the training and test data
library(caret)
set.seed(100)
train_rows <- createDataPartition(iris_n$Species, p=0.7, list=F)
train <- iris_n[train_rows, ]
test <- iris_n[-train_rows, ]

library(class)  # for the knn function
out <- knn(train[, -5], test[, -5], train[, 5], k=5)
out


fit <- caret::knn3(train[, -5], train[, 5], k=5)

# summarize the fit
print(fit)

# make predictions
predictions <- predict(fit, test[, -5], type="class")
tab <- table(predictions, test$Species)
caret::confusionMatrix(tab)

mean(test[, 5] != out)  

# KNN Regression ---------------------------------------------------
# KNN for Regression with caret
# read.csv("https://goo.gl/N5T9zC")
# FULL URL: https://raw.githubusercontent.com/selva86/datasets/master/BostonHousing.csv

data(Boston, package="MASS")
head(Boston)


set.seed(100)
# Prepare training and test data
train_rows <- caret::createDataPartition(Boston$medv, p=0.7, list=F)
train <- Boston[train_rows, ]
test <- Boston[-train_rows, ]

fit <- knnreg(train[, -14], train[, 14], k=5)
# summarize the fit
print(fit)
# make predictions
predictions <- predict(fit, test[, -14])
DMwR::regr.eval(test$medv, predictions)



# --------------------------------------------------------------------------
# Answer
# Create the training and test data
library(caret)
normalize <- function(x){
  return((x-min(x))/(max(x) - min(x)))
}

normalized_iris <- lapply(iris[, -5], normalize)
iris_n <- data.frame(normalized_iris, Species=iris[, 5])

set.seed(100)
train_rows <- createDataPartition(iris_n$Species, p=0.7, list=F)
train <- iris_n[train_rows, ]
test <- iris_n[-train_rows, ]


k_vals <- c(5, 7, 11 ,13, 17, 19)
kappa_out <- numeric(6)

for(i in 1:length(k_vals)){
  out <- knn(train[, -5], test[, -5], train[, 5], k=k_vals[i])
  
  # evaluation
  tab <- table(out, test$Species)
  mat <- caret::confusionMatrix(tab)
  kappa_out[i] <- mat$overall[2]
}

k_vals[which.max(kappa_out)]

