# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co


# Import Data
# GlaucomaM <- read.csv("https://goo.gl/YGAVYL")
# FULL URL: https://raw.githubusercontent.com/selva86/datasets/master/GlaucomaM.csv
data("GlaucomaM", package = "TH.data")
GlaucomaM

# Split training and test data
library(caret)
set.seed(100)
train_rows <- createDataPartition(GlaucomaM$Class, p=0.7, list=F)
trainData <- GlaucomaM[train_rows, ]
testData <- GlaucomaM[-train_rows, ]

## Feature Selection with Boruta ---------------------------------------------
# install.packages("Boruta")
library(Boruta)
borutaMod <- Boruta(Class ~ ., data=trainData, doTrace=1)  # perform Boruta search
borutaMod

# Retain confirmed and tentative.
boruta_signif <- getSelectedAttributes(borutaMod, 
                                       withTentative = TRUE)
print(boruta_signif)  # significant variables

# Do a rough fix on tentative variables.
roughFixMod <- TentativeRoughFix(borutaMod)
boruta_signif <- getSelectedAttributes(roughFixMod,
                                       withTentative = TRUE)
boruta_signif

plot(roughFixMod, cex.axis=.7, las=2, xlab="", 
     main="Variable Importance")  # plot variable importance

imps <- attStats(roughFixMod)
imps
imps_sort <- imps[order(-imps$meanImp), ]
rownames(imps_sort)

## Feature Selection with Var Imp -----------------------------------------
rPartMod <- train(Class ~ ., data=trainData, method="rpart")  # rpart
rpartVar <- varImp(rPartMod)
rpartVar

# RF
rfMod <- train(Class ~ ., data=trainData, method="rf")  # random forest
rfVar <- varImp(rfMod)
rfVar


## Feature Selection with RFE ----------------------------------------------
# Initiate parallel processing with 2 cores
library(doMC)
registerDoMC(cores = 2)
x <- trainData[, -63]
y <- trainData[, 63]
testX  <- testData[, -63]
# Number of features to be retained
subsets <- c(1:5, 10, 15, 20, 25, 35, 45 ,55)

# Remove highly correlated predictors.
correls = findCorrelation(cor(x), cutoff = .9)
if (length(correls) != 0){
  x <- x[,-correls]
}

# Create folds.
set.seed(100)
index <- createFolds(y, k = 10, returnTrain = T)

# Define rfeControl
ctrl <- rfeControl(functions = rfFuncs,  # caretFuncs, nbFuncs, treebagFuncs, lmFuncs
                   method = "repeatedcv",
                   repeats = 5,
                   index=index,
                   verbose = TRUE)

# Run RFE
rfProfile <- rfe(x=x, y=y,
                 sizes=subsets,
                 rfeControl = ctrl)

rfProfile 
varImp(rfProfile)  # importances of variables
names(rfProfile)
rfProfile$optVariables  # important variables 







# Answer
# Split training and test data
library(caret)
set.seed(100)
data("segmentationData", package="caret")
train_rows <- createDataPartition(segmentationData$Class, p=0.7, 
                                  list=F)
trainData <- segmentationData[train_rows, ]
testData <- segmentationData[-train_rows, ]
x <- trainData[, -c(1,2,3)]
y <- trainData[, 3]
testX <- testData[, -c(1,2,3)]

# Boruta
library(Boruta)
borMod <- Boruta(Class ~ ., data = trainData, doTrace=2)
roughFixMod <- TentativeRoughFix(borMod)
boruta_signif <- getSelectedAttributes(roughFixMod, 
                                       withTentative = TRUE)
boruta_signif

plot(roughFixMod, cex.axis=.7, las=2, xlab="", 
     main="Variable Importance")  # plot variable importance
imps <- attStats(roughFixMod)
imps
imps_sort <- imps[order(-imps$meanImp), ]
rownames(imps_sort)[1:10]

# RFE
# Initiate parallel processing with 4 cores
library(doMC)
registerDoMC(cores = 4)

# Number of features to be retained
subsets <- c(1:5, 10, 15, 20, 50)

ctrl <- rfeControl(functions = rfFuncs,  # caretFuncs, nbFuncs, treebagFuncs, lmFuncs
                   method = "repeatedcv",
                   repeats = 5,
                   index=index,
                   verbose = FALSE)

rfProfile <- rfe(x=x, y=y,
                 newData=testX,
                 sizes = subsets,
                 rfeControl = ctrl)


rfProfile
varImp(rfProfile)
rfProfile$optVariables[1:10]

rownames(imps_sort)[1:10][rownames(imps_sort)[1:10] %in% rfProfile$optVariables[1:10]]