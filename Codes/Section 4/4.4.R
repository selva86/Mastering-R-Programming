# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co


# Setup
library(glmnet)
library(caret)
options(scipen=999)
'%ni%' <- Negate('%in%')

# Create training and test Data
prostate <- read.csv("https://goo.gl/qmrDcY")
set.seed(100)
trainRows <- createDataPartition(prostate$lpsa,p=.7,list=FALSE)
trainData <- prostate[trainRows, ]
testData  <- prostate[-trainRows, ]

train_x <- as.matrix(trainData[, colnames(trainData) %ni% "lpsa"])
train_y <- as.matrix(trainData[, "lpsa"])

test_x <- as.matrix(testData[, colnames(trainData) %ni% "lpsa"])
test_y <- as.matrix(testData[, "lpsa"])

# Step 1
grid=10^seq(10,-2,length=100)
grid

# Step 2
ridgeMod = glmnet(train_x, train_y, alpha=0, 
                  lambda=grid, thresh =1e-12)
ridgeMod

# Step 3
set.seed(100)
cv.out=cv.glmnet(train_x, train_y, alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
log(bestlam)

# Step 4
pred <- predict(ridgeMod, s=bestlam, newx=test_x)
DMwR::regr.eval(test_y, pred) # mape=33.5%

# plot
coefs_ridge <- predict(ridgeMod, type="coefficients", s=bestlam)
coefs_ridge
plot(ridgeMod, xvar="lambda")



## Lasso
set.seed(100)
lassoMod = glmnet(train_x, train_y, alpha=1, lambda=grid, 
                  thresh =1e-12)
cv.out=cv.glmnet(train_x, train_y, alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam

# Predict
pred <- predict(lassoMod, s=bestlam, newx=test_x)
DMwR::regr.eval(test_y, pred)

# Examine coefficients and plot
coefs_lasso <- predict(lassoMod, type="coefficients", s=bestlam)
coefs_lasso
plot(lassoMod, xvar="lambda")


# Answer
# Setup
library(glmnet)
library(caret)
options(scipen=999)
'%ni%' <- Negate('%in%')

# Create training and test Data
data("prostate", package="ElemStatLearn")
# prostate <- read.csv("https://goo.gl/qmrDcY")
set.seed(100)
trainRows <- createDataPartition(prostate$lpsa,p=.7,list=FALSE)
trainData <- prostate[trainRows, ]
testData  <- prostate[-trainRows, ]

train_x <- as.matrix(trainData[, colnames(trainData) %ni% "lpsa"])
train_y <- as.matrix(trainData[, "lpsa"])
test_x <- as.matrix(testData[, colnames(trainData) %ni% "lpsa"])
test_y <- as.matrix(testData[, "lpsa"])

alphas <- seq(0,1,by=0.01)
alphas

set.seed(100)
foldid <- sample(1:10,size=length(train_y),replace=TRUE)  # define the foldsid
grid <- 10^seq(10,-2,length=100)
mapes <-numeric(length(alphas))  # initialize output
i = 1  # loop counter.
for(a in alphas){
  bestlam <- cv.glmnet(train_x, train_y, alpha=a, lambda=grid, 
                       foldid = foldid)$lambda.min  # get best lambda for given alpha 'a'.
  
  enetMod <- glmnet(train_x, train_y, alpha=a, lambda=bestlam)  # fit glmnet model
  
  pred <- predict(enetMod, s=bestlam, newx=test_x)  # predict
  
  mapes[i] <- DMwR::regr.eval(test_y, pred)[4]  # get MAPE
  
  i = i + 1  # increment loop counter.
} 
out <- cbind(alphas, mapes)  # final alphas and best MAPE

# Plot
plot(out, type="l", col="blue")
alpha <- out[which.min(out[, 2]), 1]
mape <- out[which.min(out[, 2]), 2]
points(x=alpha, y=mape, cex=2, col="red", pch="*")
alpha
























