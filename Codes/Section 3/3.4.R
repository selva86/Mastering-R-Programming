# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co


# Split training and test data
library(caret)
set.seed(100)
train_rows <- createDataPartition(iris$Species, p=0.7, list=F)
trainData <- iris[train_rows, ]
testData <- iris[-train_rows, ]

# cTree Model -----------------------------------------------------------------------
library(partykit)
ctMod <- ctree(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
               data=trainData)
print(ctMod)
plot(ctMod)


# cTree model with control parameter -----------------------------------------------
ctMod_2 <- ctree(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
                 control=ctree_control(maxdepth = 2),
                 data=trainData)
print(ctMod_2)
plot(ctMod_2)

# Predict
# Predict
out <- predict(ctMod_2, testData)

# Compare
mean(testData[, 5] != out)  
sum(testData[, 5] != out)  


# RPart Model -----------------------------------------------------------------------
library(rpart)
rpartMod <- rpart(Species ~ ., data=trainData, 
                  control=rpart.control(minsplit=5, cp=0, 
                                        maxdepth = 4))
rpartMod

pred <- predict(rpartMod, testData, type="class")
pred
mean(pred != as.character(testData$Species))  

library(rattle)
library(RColorBrewer)
fancyRpartPlot(rpartMod)  # rattle lib


# Make cTree Plot from RPart Model
library(partykit)
iris_party <- as.party.rpart(rpartMod)
plot(iris_party)

# Make rpart.plot
library(rpart.plot)
prp(rpartMod, extra=1, type=2)  

# Prune the tree
rpartmod_pruned <- prp(rpartMod,snip=TRUE)$obj # interactively trim the tree

prp(rpartmod_pruned, extra=1, type=2) 


# C5.0 Model -----------------------------------------------------------------------
library(C50)
c5Mod <- C5.0(Species ~ ., data=trainData, 
              control=C5.0Control(winnow=F))

summary(c5Mod)
plot(c5Mod)
C5imp(c5Mod) # importance of variables


# C5.0 Rules model
c5Mod_rules <- C5.0(Species ~ ., data=trainData, rules=T, 
                    control=C5.0Control(winnow=F))
summary(c5Mod_rules)




# -----------------------------------------------------------------------
# Answer
# Prepare the data
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

# Ctree mod
library(partykit)
ctMod <- ctree(Class ~ ., data = trainData)
print(ctMod)
plot(ctMod)
pred_ctree <- predict(ctMod, testData)
mean(pred_ctree != testData$Class) 


# C5 Mod
library(C50)
c5Mod <- C5.0(Class ~ ., data = trainData)
summary(c5Mod)
plot(c5Mod, gp = gpar(fontsize = 6))  # reduce the font size
pred_c5 <- predict(c5Mod, testData)
mean(pred_c5 != testData$Class)  

