# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co


# Setup
library(caret)
library(doMC)
registerDoMC(cores = 4)

# Load the data and create training and test samples.
data(segmentationData, package = "caret")
set.seed(100)
trainRows <- createDataPartition(segmentationData$Class,p=.7,list=FALSE)
trainData <- segmentationData[trainRows, -c(1:2)]
testData  <- segmentationData[-trainRows, -c(1:2)]

# setup trainControl
ctrl <- trainControl(method="repeatedcv",   # 10 fold cross validation
                     repeats=5,		    # 5 repeats of cv
                     summaryFunction=multiClassSummary,	# To get AUROC
                     classProbs=TRUE)

ncol(trainData)
grid <- expand.grid(mtry = c(2, 8, 15, 20, 30)) 

system.time({
  parRF_mod <- train(Class ~ ., data = trainData, 
                     method = "parRF",  # ranger
                     preProc = c("center","scale"),
                     metric="ROC",
                     tuneGrid=grid,
                     trControl=ctrl)	  
})

parRF_mod
pred <- predict(parRF_mod, testData)
caret::confusionMatrix(pred, testData$Class)



# Answer
source("https://goo.gl/ZniFaV")
set.seed(100)                     

# setup trainControl
ctrl <- trainControl(method="repeatedcv",   # 10 fold cross validation
                     repeats=5,		    # 5 repeats of cv
                     summaryFunction=multiClassSummary,	# To get AUROC
                     classProbs=TRUE)

# Train and Tune the random forest

grid <- expand.grid(mtry = c(2, 8, 15, 20, 30))
grid


system.time({
  cforest_mod <- train(Class ~ ., data = trainData, 
                       method = "cforest",
                       preProc = c("center","scale"),
                       metric="ROC",
                       tuneGrid=grid,
                       trControl=ctrl)	  
})


modelLookup('cforest')

cforest_mod
pred <- predict(cforest_mod, testData)
caret::confusionMatrix(pred, testData$Class)
