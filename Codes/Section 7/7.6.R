# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co


library(RTextTools)
docs <- read.csv("https://goo.gl/9gO9jU", stringsAsFactors = F)  
# alt source: https://goo.gl/kjyT8K
# data URL: "https://archive.ics.uci.edu/ml/machine-learning-databases/00233/CNAE-9.data"
# Data desc url: https://archive.ics.uci.edu/ml/datasets/CNAE-9

## Bypassed DTM creation.
# doc_matrix <- create_matrix(text, language="english", removeNumbers=TRUE, 
#                              stemWords=TRUE, removeSparseTerms=.998)

dim(docs)
doc_matrix <- as.matrix(docs[, -1])
container <- create_container(doc_matrix, docs[, 1], trainSize=1:900, 
                              testSize=901:1079, virgin=FALSE)
container

# ML Models Training
SVM <- train_model(container,"SVM")
GLMNET <- train_model(container,"GLMNET")
MAXENT <- train_model(container,"MAXENT")
SLDA <- train_model(container,"SLDA")
BOOSTING <- train_model(container,"BOOSTING")
BAGGING <- train_model(container,"BAGGING")
RF <- train_model(container,"RF")
NNET <- train_model(container,"NNET")
TREE <- train_model(container,"TREE")

actuals <- tail(docs[, 1], 179)
actuals

# predict
SVM_CLASSIFY <- classify_model(container, SVM)
GLMNET_CLASSIFY <- classify_model(container, GLMNET)
MAXENT_CLASSIFY <- classify_model(container, MAXENT)
SLDA_CLASSIFY <- classify_model(container, SLDA)
BOOSTING_CLASSIFY <- classify_model(container, BOOSTING)
BAGGING_CLASSIFY <- classify_model(container, BAGGING)
RF_CLASSIFY <- classify_model(container, RF)
NNET_CLASSIFY <- classify_model(container, NNET)
TREE_CLASSIFY <- classify_model(container, TREE)

# Misclass Rate
mean(as.character(SVM_CLASSIFY$SVM_LABEL) != actuals)
mean(as.character(GLMNET_CLASSIFY$GLMNET_LABEL) != actuals)
mean(as.character(MAXENT_CLASSIFY$MAXENTROPY_LABEL) != actuals)
mean(as.character(SLDA_CLASSIFY$SLDA_LABEL) != actuals)
mean(as.character(BOOSTING_CLASSIFY$LOGITBOOST_LABEL) != actuals)
mean(as.character(BAGGING_CLASSIFY$BAGGING_LABEL) != actuals)
mean(as.character(RF_CLASSIFY$FORESTS_LABEL) != actuals)
mean(as.character(NNET_CLASSIFY$NNETWORK_LABEL) != actuals)
mean(as.character(TREE_CLASSIFY$TREE_LABEL) != actuals)


