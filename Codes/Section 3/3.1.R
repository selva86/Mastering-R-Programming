# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co


# BreastCancer <- read.csv("https://goo.gl/IClpPh")
# FULL URL: "https://raw.githubusercontent.com/selva86/datasets/master/BreastCancer.csv"
# install.packages("mlbench")

data(BreastCancer, package="mlbench")
bc<- BreastCancer[complete.cases(BreastCancer), ]  # create copy
dim(bc)
str(bc)

bc<- bc[,-1]

# convert to numeric
for(i in 1:9) {
  bc[,i] <- as.numeric(as.character(bc[,i]))
}

bc$Class <- ifelse(bc$Class == "malignant", 1, 0)
bc$Class <- factor(bc$Class, levels = c(0, 1))
table(bc$Class)

# Train & Test
library(caret)
'%ni%' <- Negate('%in%') # define 'not in' func

options(scipen=999)
# prevents printing scientific notations.

# Prep Training and Test data.
set.seed(100)
trainDataIndex<- createDataPartition(bc$Class, p=0.7, list = F)
trainData<- bc[trainDataIndex, ]
testData<- bc[-trainDataIndex, ]



# Downsampling
set.seed(100)
down_train <- downSample(x = trainData[, colnames(trainData) %ni% "Class"],
                         y = trainData$Class)

table(down_train$Class)


# Upsampling
set.seed(100)
up_train<- upSample(x = trainData[, colnames(trainData) %ni% "Class"],
                    y = trainData$Class)

table(up_train$Class)

# Hybrid
DMwR::SMOTE
ROSE::ROSE

# Logit Model
logitmod<- glm(Class ~ Cl.thickness + Cell.size + Cell.shape, 
               family = binomial, data=down_train)

summary(logitmod)

pred<- predict(logitmod, newdata = testData, type = "response")

y_pred_num<- ifelse(pred > 0.5, 1, 0)
y_pred<- factor(y_pred_num, levels=c(0, 1))
y_act <- testData$Class

y_pred
y_act

# Accuracy
mean(y_pred == y_act)

caret::confusionMatrix(y_pred, y_act, positive="1")

"https://en.wikipedia.org/wiki/Confusion_matrix"


library(InformationValue)
InformationValue::plotROC(y_act, pred)
InformationValue::AUROC(y_act, pred)
InformationValue::Concordance(y_act, pred)



# Answer
source("https://goo.gl/UXRthP")

# Or type
BreastCancer <- read.csv("https://raw.githubusercontent.com/selva86/datasets/master/BreastCancer.csv")
bc<- BreastCancer[complete.cases(BreastCancer), ]  # retain complete cases

bc$Class <- factor(bc$Class, levels = c(0, 1))  # convert to factor

library(caret)
'%ni%' <- Negate('%in%')  # define 'not in' func
options(scipen=999)  # prevents printing scientific notations.

# Prep Training and Test data.
set.seed(100)
trainDataIndex<- createDataPartition(bc$Class, p=0.7, list = F)
trainData<- bc[trainDataIndex, ]
testData<- bc[-trainDataIndex, ]

### Down Sample
set.seed(100)
down_train <- downSample(x = trainData[, colnames(trainData) %ni% "Class"],
                         y = trainData$Class)

### Build model
mod1<- glm(Class ~ Cl.thickness + Mitoses + Bl.cromatin, family = binomial, data=down_train)
summary(mod1)

mod2<- glm(Class ~ Cell.size + Marg.adhesion, family = binomial, data=down_train)
summary(mod2)

# Predict - mod1
pred1<- predict(mod1, newdata = testData, type = "response")
y_pred_num<- ifelse(pred1 > 0.5, 1, 0)
y_pred1 <- factor(y_pred_num, levels=c(0, 1))
y_act <- testData$Class
y_pred1
y_act

# Predict - mod2
pred2<- predict(mod2, newdata = testData, type = "response")
y_pred_num<- ifelse(pred2 > 0.5, 1, 0)
y_pred2 <- factor(y_pred_num, levels=c(0, 1))
y_act <- testData$Class
y_pred2
y_act

# Confusion Mat
# Mod1 - Compute the confusion matrix and auroc
caret::confusionMatrix(y_pred1, y_act, positive="1")
InformationValue::AUROC(y_act, pred1)
InformationValue::Concordance(y_act, pred1)$Concordance

# Mod2 - Compute the confusion matrix and auroc
caret::confusionMatrix(y_pred2, y_act, positive="1")
InformationValue::AUROC(y_act, pred2)
InformationValue::Concordance(y_act, pred2)$Concordance




























