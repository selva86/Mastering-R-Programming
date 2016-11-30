# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co


# Install XGBoost
install.packages("drat", repos="https://cran.rstudio.com")
drat:::addRepo("dmlc")
install.packages(c("xgboost", "Ckmeans.1d.dp", "DiagrammeR"), repos="http://dmlc.ml/drat/", type="source")
# "DiagrammeR" reqd for xgb.plot.tree
# "Ckmeans.1d.dp" reqd for xgb.plot.importance


# Setup
library(caret)
library(xgboost)
library(Matrix)
options(scipen=999)
'%ni%' <- Negate('%in%')

# Prepare the datasets
prostate <- read.csv("https://goo.gl/qmrDcY")
set.seed(100)
trainRows <- createDataPartition(prostate$lpsa,p=.7,list=FALSE)
trainData <- prostate[trainRows, ]
testData  <- prostate[-trainRows, ]

# creating the matrix for training the model
trainData_xg <- xgb.DMatrix(data.matrix(trainData[, colnames(trainData) %ni% 'lpsa']), 
                            label = as.numeric(trainData$lpsa))

testData_xg <- xgb.DMatrix(data.matrix(testData[, colnames(testData) %ni% 'lpsa']))

watchlist <- list(train = trainData_xg, test = testData_xg)


# Define the parameters and cross validate
param <- list("objective" = "reg:linear",
              "eval_metric" = "rmse")
cv.nround <- 5
cv.nfold <- 3
cvMod <- xgb.cv(param=param, data = trainData_xg,
                nfold = cv.nfold, 
                nrounds = cv.nround)
cvMod

# Build the XGBoost model
nrounds <- 50
xgMod <- xgb.train(param=param, data = trainData_xg, 
                   nrounds=nrounds, booster="gblinear")

# Predict
pred <- predict(xgMod, testData_xg)

# MAPE
DMwR::regr.eval(testData$lpsa, pred)

## Caret
library(caret)
library(doMC)
registerDoMC(cores = 4)

# pack the training control parameters
xgb_trcontrol = trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all",         # save losses across all models
  allowParallel = TRUE)

modelLookup('xgbLinear')


# Tune the model
set.seed(100)
xgb_train = train(
  x = as.matrix(trainData[, colnames(trainData) %ni% "lpsa"]),
  y = trainData$lpsa,
  trControl = xgb_trcontrol,
  tuneLength=3,
  method = "xgbLinear"
)

# Predict
pred <- predict(xgb_train, testData)

# MAPE
DMwR::regr.eval(testData$lpsa, pred)





# Answer
# Prepare the datasets
data(Glass, package = "mlbench")
# Glass <- read.csv("https://goo.gl/sdIBpS")
levels(Glass$Type) <- c(0:5)  # proper sequence. Should start from 0.
Glass$Type <- as.integer(as.character(Glass$Type))  # convert to integer 
unique(Glass$Type)

set.seed(100)
trainRows <- createDataPartition(Glass$Type,p=.7,list=FALSE)
trainData <- Glass[trainRows, ]
testData  <- Glass[-trainRows, ]

# creating the matrix for training the model
trainData_xg <- xgb.DMatrix(data.matrix(trainData[, colnames(trainData) %ni% 'Type']), 
                            label = trainData$Type)

testData_xg <- xgb.DMatrix(data.matrix(testData[, colnames(testData) %ni% 'Type']))

watchlist <- list(train = trainData_xg, test = testData_xg)

# Define the parameters and cross validate
param <- list("objective" = "multi:softmax",
              "eval_metric" = "mlogloss",
              "num_class" = length(unique(trainData$Type)))
cv.nround <- 5
cv.nfold <- 3
cvMod = xgb.cv(param=param, data = trainData_xg, nfold = cv.nfold, 
               nrounds = cv.nround, watchlist=watchlist)

# Build
# Build the XGBoost model
nrounds = 50
xgMod = xgboost(param=param, data = trainData_xg, nrounds=nrounds, watchlist=watchlist)

# See the XGBoost Model
model <- xgb.dump(xgMod, with.stats = T)
model[1:10]

# Feature Importance
# Feature Importance
names <- colnames(trainData)[colnames(trainData) %ni% 'lpsa']  # Get the real names

# Compute feature importance matrix
featureImp <- xgb.importance(names, model = xgMod)
featureImp

# Nice graph
xgb.plot.importance(featureImp)

# Plot
xgb.plot.tree(feature_names=names, 
              model=xgMod, n_first_tree = 2)


# Predict
pred <- predict(xgMod, testData_xg)

tab <- table(pred, testData$Type)
caret::confusionMatrix(tab)
