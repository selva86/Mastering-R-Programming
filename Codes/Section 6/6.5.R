# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co


# Double Exponential Smoothing (Or just holt)
library(forecast)
data("ausair", package="fpp")
# data download url: "https://github.com/selva86/datasets/blob/master/ausair.RDS"
hfit1 <- holt(ausair, initial="simple")
summary(hfit1)

# method 2
hfit2 <- holt(ausair, initial="simple", exponential = T)
summary(hfit2)

#  method 3
hfit3 <- holt(ausair, damped = T)
summary(hfit3)


plot(ausair, main="Air Passengers - Australia", type="o", xlim=c(1970, 2019), 
     ylim=c(0, 70))
lines(fitted(hfit1), col="red", lty=2)  # holt fitted
lines(fitted(hfit2), col="green", lty=2)  # exponential fitted
lines(fitted(hfit3), col="blue", lty=2)  # damped fitted
lines(hfit1$mean, col="red", type="o")  # holt forecast
lines(hfit2$mean, col="green", type="o")  # exponential forecast
lines(hfit3$mean, col="blue", type="o")  # damped forecast


# Holtwinters
hw_fit1 <- hw(JohnsonJohnson, seasonal="additive")
hw_fit2 <- hw(JohnsonJohnson, seasonal="multiplicative")
summary(hw_fit1)

# plot
plot(JohnsonJohnson, type="o", xlim=c(1960, 1983), ylim=c(0, 20), 
     main="JohnsonJohnson")
lines(fitted(hw_fit1), col="blue", lty=2)  # damped fitted
lines(fitted(hw_fit2), col="red", lty=2)  # damped fitted
lines(hw_fit1$mean, col="blue", type="o")  # holt forecast
lines(hw_fit2$mean, col="red", type="o")  # holt forecast


# Answer
train <- window(AirPassengers, start=c(1949,1), end=c(1958, 12))
test <- window(AirPassengers, start=c(1959,1), end=c(1960, 12))

fit <- hw(train, seasonal = "additive")
predicted <- forecast(fit, h=24)$mean
accuracy(predicted, test)































