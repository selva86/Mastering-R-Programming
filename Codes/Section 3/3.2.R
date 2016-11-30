# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co


data(Vehicle, package="mlbench")  # load the data
# Vehicle <- read.csv("https://goo.gl/N0PZtY")
# FULL URL: https://raw.githubusercontent.com/selva86/datasets/master/Vehicle.csv

head(Vehicle)  # top 6 rows
str(Vehicle)

# Create training and test
library(caret)
set.seed(100)
train_rows<- caret::createDataPartition(Vehicle$Class, p=0.7, list=F)
train <- Vehicle[train_rows, ]
test <- Vehicle[-train_rows, ]

caret::featurePlot(Vehicle[, -19], Vehicle[, 19], plot = "box")


# Using klaR for Naive Bayes
library(klaR)
nb_mod<- NaiveBayes(Class ~ ., data=train)
pred<- predict(nb_mod, test)
mean(test$Class != pred$class)  

# Confusion Matrix
tab <- table(pred$class, test$Class)
caret::confusionMatrix(tab)  


plot(nb_mod)



# Answer
library(caret)
set.seed(100)
train_rows<- caret::createDataPartition(iris$Species, p=0.7, list=F)
train <- iris[train_rows, ]
test <- iris[-train_rows, ]

# Feature Plot
caret::featurePlot(iris[, -5], iris[, 5], plot = "box")

# Using klaR for Naive Bayes
library(klaR)
nb_mod<- NaiveBayes(Species ~ ., data=train)
pred<- predict(nb_mod, test)
mean(test$Species != pred$class)  # 0.08888889

# Confusion Matrix
tab <- table(pred$class, test$Species)
caret::confusionMatrix(tab)

plot(nb_mod)
# Shows the density of the continuous X variable after splitting for individual classes.

























