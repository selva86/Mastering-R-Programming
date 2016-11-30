# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co


data(Prestige, package="car")
head(Prestige)

set.seed(100)
train_rows<- sample(1:nrow(Prestige), size=0.7*nrow(Prestige))
train_rows
training <- Prestige[train_rows, ]
test <- Prestige[-train_rows, ]
dim(training)
dim(test)

lmmod<- lm(prestige ~ income + education, data=training)
lmmod

names(lmmod)
lmmod[1]
lmmod[2]

summary(lmmod)

library(broom)
lmmod_df <- tidy(lmmod)
lmmod_df

predicted <- predict(lmmod, newdata = test)

# MSE: Mean Squared Error
mean((test$prestige - predicted)^2)

# MAPE: Mean Absolute Percentage Error
mean(abs(test$prestige - predicted)/test$prestige)

# Answer:
predicted <- predict(lmmod, newdata = training)
mean((training$prestige - predicted)^2)
mean(abs(training$prestige - predicted)/training$prestige)

DMwR::regr.eval(training$prestige, predicted)













