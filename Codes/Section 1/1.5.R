# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co

Prestige_miss<- read.csv("https://raw.githubusercontent.com/selva86/datasets/master/Prestige_miss.csv")
myData <- Prestige_miss
head(myData)

library(Hmisc)
myData$education<- impute(myData$education, mean)
myData$type<- impute(myData$type, mode)

library(mice)
myData <- Prestige_miss
micemod<- mice(myData)

myData2 <- complete(micemod, 1)
myData2 <- complete(micemod, 2)
