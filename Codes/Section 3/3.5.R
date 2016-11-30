# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co


# Load the package
library(caret)

# Data PreProcessing -----------------------------------------------------------
# Use preProcess() to normalize all X's to range from 0 and 1.
preprocessParams <- preProcess(iris[,1:4], method=c("range"))
preprocessParams

# Use predict() to apply the normalization
normalized <- predict(preprocessParams, iris[,1:4])
iris_n <- cbind(normalized, Species=iris$Species)

# Summarize
summary(iris_n)

# Training 1: Most simple `train()` --------------------------------------------------
# Prepare data, train() and predict
set.seed(100)
train_rows <- createDataPartition(iris_n$Species, p=0.7, list=F)
trainData <- iris_n[train_rows, ]
testData <- iris_n[-train_rows, ]
fit <- train(Species ~ ., data=trainData, preProcess=c("range"), 
             method="knn")

predict(fit$finalModel, newdata = testData[, 1:4], type="class")

# Training 2: Add trainControl() so as to use with `trControl` option. -------------- 
tc <- trainControl(method="repeatedcv", number=5, repeats=3)
tc

# Simple training.
fit_repeatcv <- train(Species ~ ., data=trainData, 
                      preProcess=c("range"), 
                      method="knn", 
                      trControl=tc)
fit_repeatcv

# Training 3: Introduce the evaluation metric to choose best model-------------------- 
# train() with kappa as evaluation metric.
fit_repeatcv_kappa <- train(Species ~ ., data=trainData, 
                            preProcess=c("range"), 
                            method="knn", 
                            trControl=tc, 
                            metric="Kappa")
fit_repeatcv_kappa

# Training 4: Add multiClassSummary since response var is multiclass ----------- 
# Add MultiClass Summary to trainControl
tc <- trainControl(method="repeatedcv", 
                   number=5, 
                   repeats=3, 
                   summaryFunction=multiClassSummary)

fit_repeatcv_kappa_summary <- train(Species ~ ., 
                                    data=trainData, 
                                    preProcess=c("range"), 
                                    method="knn", 
                                    trControl=tc, 
                                    metric="Kappa")
fit_repeatcv_kappa


# Find Tuning Params -----------------------------------------------------------
# Lookup tuning parameters for knn
modelLookup("knn")


# Training 5: train() with search grid for tuning parameters ------------------ 
# Define search grid
grid <- expand.grid(k=c(5, 7, 11 ,13, 17, 19, 23, 25))
fit_repeatcv_kappa_tunegrid <- train(Species ~ ., 
                                     data=trainData, 
                                     preProcess=c("range"), 
                                     method="knn", 
                                     trControl=tc, 
                                     metric="Kappa", 
                                     tuneGrid=grid)
fit_repeatcv_kappa_tunegrid
fit_repeatcv_kappa_tunegrid$finalModel


# Training 6: train() with tuneLength(). Caret automatically set search grid ---- 
# Define tuneLength
tc <- trainControl(method="repeatedcv", 
                   number=5, repeats=3, 
                   search="random", 
                   summaryFunction=multiClassSummary)
knn_fit_repeatcv_kappa <- train(Species ~ ., data=trainData, 
                                preProcess=c("range"), 
                                method="knn", trControl=tc, 
                                metric="Kappa", tuneLength=20)
knn_fit_repeatcv_kappa


# Training 7: train() a C5.0 model and predict
c5_fit_repeatcv_kappa <- train(Species ~ ., 
                               data=trainData, 
                               preProcess=c("range"), 
                               method="C5.0", 
                               trControl=tc, 
                               metric="Kappa", 
                               tuneLength=10)

c5Fit <- c5_fit_repeatcv_kappa$finalModel
out <- predict(c5Fit, testData, type="class")
caret::confusionMatrix(out, testData$Species)


# Multiclass summary on test Data.
testResults <- predict(c5Fit, testData, type = "prob")  # class probabilities
testResults <- data.frame(testResults)  # convert to dataframe.
testResults$obs <- testData$Species  # assign actual classes
testResults$pred <- predict(c5Fit, testData, type="class")  # assign predicted classes 
multiClassSummary(testResults, lev = levels(testData$Species))

# Training 8: train() a cTree model and predict -------------------------------
# ctree model
ctree_fit_repeatcv_kappa <- train(Species ~ ., 
                                  data=trainData, 
                                  preProcess=c("range"), 
                                  method="ctree", 
                                  trControl=tc, 
                                  metric="Kappa", 
                                  tuneLength=10)

# Compare models with resamples()  --------------------------------------------
combo <- resamples(list(KNN=knn_fit_repeatcv_kappa, 
                        C5=c5_fit_repeatcv_kappa, 
                        cTree=ctree_fit_repeatcv_kappa))

summary(combo)

# Use bwplot() to compare models visually -------------------------------------
# Dot plots of accuracy
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(combo, scales=scales)





# -----------------------------------------------------------------------------
# Answer
# Create training and test
library(caret)
set.seed(100)
data(Vehicle, package="mlbench")  # load the data
train_rows <- caret::createDataPartition(Vehicle$Class, 
                                         p=0.7, list=F)
trainData <- Vehicle[train_rows, ]
testData <- Vehicle[-train_rows, ]

# parallelization
library(doMC)
registerDoMC(cores = 4)
tc <- trainControl(method="repeatedcv", 
                   number=5, repeats=3, 
                   search="random", 
                   summaryFunction=multiClassSummary)

fit <- train(Class ~ ., data=trainData, 
             method="C5.0", trainControl=tc, 
             metric="Kappa", tuneLength=5)
mod <- fit$finalModel
pred <- predict(mod, testData)

# Compute Accuracy
testResults <- predict(mod, testData, type = "prob")  # class probabilities
testResults <- data.frame(testResults)  # convert to dataframe.
testResults$obs <- testData$Class  # assign actual classes
testResults$pred <- predict(fit, testData, type="raw")  # assign predicted classes 
multiClassSummary(testResults, lev = levels(testData$Class))
