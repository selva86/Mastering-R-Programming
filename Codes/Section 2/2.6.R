# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co


# Data Prep
data(Prestige, package="car")
Prestige <- na.omit(Prestige)
set.seed(100)
train_rows<- sample(1:nrow(Prestige), size=0.7*nrow(Prestige))
training <- Prestige[train_rows, ]
test <- Prestige[-train_rows, ]
plot(x=training$income, y=training$prestige)

# Smoothing Splines
sp_2 <- smooth.spline(x=training$income, y=training$prestige, df=2)
sp_10 <- smooth.spline(x=training$income, y=training$prestige, df=10)
sp_20 <- smooth.spline(x=training$income, y=training$prestige, df=20)
sp_50 <- smooth.spline(x=training$income, y=training$prestige, df=50)
sp_cv <- smooth.spline(x=training$income, y=training$prestige, cv=T)
sp_cv$df

# Plot spline output
plot(x=Prestige$income, y=Prestige$prestige, main = "Income vs Prestige")
lines(sp_2, col="blue",lwd=2)
lines(sp_20, col="red",lwd=2)
lines(sp_50, col="orange",lwd=2)
lines(sp_cv, col="black",lwd=3)

# Predict
predicted <- predict(sp_cv, test$income)$y
DMwR::regr.eval(test$prestige, predicted)

# Natural Splines
library(splines)
head(ns(Prestige$income, df=3))

# GAMs
library(mgcv)
gamMod<- mgcv::gam(prestige ~ ns(income, 3) + ns(education, 4) +  type, data=training)
predicted <- predict(gamMod, test)
DMwR::regr.eval(test$prestige, predicted)

cars1 <- cars[1:30, ]  # original data
cars_outliers<- data.frame(speed=c(19,19,20,20,20), dist=c(190, 186, 210, 220, 218))  # introduce outliers.
cars2 <- rbind(cars1, cars_outliers)  # data with outliers.
plot(cars2$speed, cars2$dist, pch="*", col="red", main="Dist Vs. Speed", xlab="Speed", ylab="Dist", cex=2)
abline(lm(dist ~ speed, data=cars2), col="blue", lwd=3, lty=2)

lmmod<- lm(dist ~ speed, data=cars2)  # fit model
predicted_lm<- predict(lmmod, cars2)  # predict
DMwR::regr.eval(cars2$dist, predicted_lm)  # Errors


# Answer to Coding Challenge
gamMod1 <- mgcv::gam(dist ~ ns(speed, 3), data=cars2)
predicted <- predict(gamMod1)
DMwR::regr.eval(cars2$dist, predicted)

plot(cars2$speed, cars2$dist, pch="*", col="red")
lines(cars2$speed, predicted)
