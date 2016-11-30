# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co

library(caret)
library(doMC)
registerDoMC(cores = 4)

# Load the data and create training and test samples.
data(Glass, package = "mlbench")
# Glass <- read.csv("https://goo.gl/sdIBpS")
Glass$Type <- make.names(Glass$Type)
set.seed(100)
trainRows <- createDataPartition(Glass$Type, p=.7,list=FALSE)
trainData <- Glass[trainRows, ]
testData  <- Glass[-trainRows, ]


modelLookup('gbm')

trainControl <- trainControl(method="repeatedcv", 
                             number=10, repeats=3, classProbs=T)
metric <- "ROC"

# Stochastic Gradient Boosting with GBM
set.seed(100)

gbmGrid <- expand.grid(interaction.depth = c(1,2), 
                       n.trees = seq(100, 1000, by =400),
                       shrinkage = c(0.01, 0.1), 
                       n.minobsinnode=c(10, 30, 50))

gbmFit <- train(Type ~ ., data=trainData, 
                method="gbm", 
                metric="Accuracy",
                trControl=trainControl, 
                tuneGrid=gbmGrid, 
                verbose=FALSE)

pred <- predict(gbmFit, testData)

caret::confusionMatrix(pred, testData$Type) 

## Boosting Regression Tree (Continuous Y Variable)--------------

data(Sacramento, package="caret")
# Sacramento <- read.csv("https://goo.gl/IKvmtC") 
set.seed(100)
trainRows <- createDataPartition(Sacramento$price, p=.7,list=FALSE)
trainData <- Sacramento[trainRows, ]
testData  <- Sacramento[-trainRows, ]

trainControl <- trainControl(method="repeatedcv", 
                             number=10, repeats=3, classProbs=F)

# Stochastic Gradient Boosting with GBM
set.seed(100)
gbmGrid <- expand.grid(interaction.depth = c(1,2,3), 
                       n.trees = seq(100, 1000, by =400),
                       shrinkage = c(0.01, 0.1, .2), 
                       n.minobsinnode=c(5, 10, 20))

gbmFit <- train(price ~ ., data=trainData, 
                method="gbm", 
                metric="RMSE",
                trControl=trainControl, 
                tuneGrid=gbmGrid, 
                verbose=FALSE)

varImp(gbmFit)

pred <- predict(gbmFit, testData)

DMwR::regr.eval(testData$price, pred)




# Answer
source("https://goo.gl/ZniFaV")
trainControl <- trainControl(method="repeatedcv", 
                             number=10, repeats=3, classProbs=T)
metric <- "ROC"

# Stochastic Gradient Boosting with GBM
set.seed(100)

gbmGrid <- expand.grid(interaction.depth = c(1,2), 
                       n.trees = seq(100, 1000, by =400),
                       shrinkage = c(0.01, 0.1), 
                       n.minobsinnode=c(10, 30, 50))

gbmFit <- train(Class ~ ., data=trainData, 
                method="gbm", metric="Accuracy",
                trControl=trainControl, 
                tuneGrid=gbmGrid, verbose=FALSE)

pred <- predict(gbmFit, testData)

caret::confusionMatrix(pred, testData$Class, positive="WS") 

