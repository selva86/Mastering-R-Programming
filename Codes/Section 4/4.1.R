# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co


# Training SVM Models
library(caret)

# Load the data and create training and test samples.
data(segmentationData, package = "caret")
# segmentationData <- read.csv("https://goo.gl/1iycql")
set.seed(100)
trainRows <- createDataPartition(segmentationData$Class,
                                 p=.7,
                                 list=FALSE)
trainData <- segmentationData[trainRows, -c(1:2)]
testData  <- segmentationData[-trainRows, -c(1:2)]
table(segmentationData$Class)

set.seed(100)                     


# SVM Linear Kernel -----------------------------------
# setup trainControl
ctrl <- trainControl(method="repeatedcv",   # 10 fold cross validation
                     repeats=5,		    # 5 repeats of cv
                     summaryFunction=twoClassSummary,	# To get AUROC
                     classProbs=TRUE,
                     sampling='down')


grid <- expand.grid(C = c(0.25, 0.5, 0.75, 1, 1.25, 1.5)) 

# Train and Tune the SVM
svmLin_mod <- train(Class ~ ., data = trainData, 
                    method = "svmLinear",
                    preProc = c("center","scale"),
                    metric="ROC",
                    tuneGrid = grid,
                    trControl=ctrl)	
svmLin_mod




# SVM - Radial Kernel -----------------------------------------
# Setup model tuning
grid <- expand.grid(sigma = c(.01, .015, 0.2),
                    C = c(0.25, 0.5, 0.75, 1, 1.25, 1.5))
grid 

# Train SVM
svmRad_mod <- train(Class ~ ., data = trainData, 
                    method = "svmRadial",
                    preProc = c("center", "scale"), 
                    metric="ROC", 
                    tuneGrid = grid,
                    trControl=ctrl)

svmRad_mod


# SVM - Polynomial Kernel -----------------------------------------
grid <- expand.grid(C = c(0.25, 0.5, 0.75, 1, 1.25, 1.5),
                    degree=c(1, 2, 3),
                    scale=c(.001, 0.01, 0.1)) 
grid

svmPoly_mod <- train(Class ~ ., data = trainData, 
                     method = "svmPoly",
                     preProc = c("center","scale"),
                     metric="ROC",
                     tuneLength = 3,
                     trControl=ctrl)	
svmPoly_mod



# Resamples (Compare models) -----------------------------------------
comparisons <- resamples(list(linear=svmLin_mod, 
                              radial=svmRad_mod, 
                              poly=svmPoly_mod))
summary(comparisons)
comparisons$values


# Plot
bwplot(comparisons, 
       metric="ROC", 
       ylab =c("Linear", "Radial", "Polynomial"))  # boxplot

# Predict
pred <- predict(svmRad_mod, testData)
caret::confusionMatrix(pred, testData$Class, positive = "WS")






# Answer
# Load the data and create training and test samples.
data(Glass, package = "mlbench")
# Glass <- read.csv("https://goo.gl/sdIBpS")
levels(Glass$Type)
levels(Glass$Type) <- make.names(levels(Glass$Type))
levels(Glass$Type)

set.seed(100)
trainRows <- createDataPartition(Glass$Type,p=.7,list=FALSE)
trainData <- Glass[trainRows, ]
testData  <- Glass[-trainRows, ]

# setup trainControl
ctrl <- trainControl(method="repeatedcv",   # 10 fold cross validation
                     repeats=5,		    # 5 repeats of cv
                     summaryFunction=multiClassSummary,	# To get AUROC
                     classProbs=TRUE)

# Setup model tuning
grid <- expand.grid(sigma = c(.01, .1, 0.2, .5, 1),
                    C = c(0.25, 0.5, 1, 5, 10, 15))

# Train SVM
svmRad_mod_cl <- train(Type ~ ., data = trainData, 
                       method = "svmRadial",
                       preProc = c("center", "scale"), 
                       metric="Mean_ROC", 
                       tuneGrid = grid,
                       trControl=ctrl)

svmRad_mod_cl

pred <- predict(svmRad_mod_cl, testData)
confusionMatrix(pred, testData$Type)

