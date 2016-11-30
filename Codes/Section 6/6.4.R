# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co


# Moving Average
library(forecast)
out <- ma(AirPassengers, 5)
plot(AirPassengers)
lines(out, col="red")

# Exponential Smoothing
out <- ses(AirPassengers, initial="simple", alpha=0.3, h=2)  # alpha =0.3
out

out1 <- ses(AirPassengers, initial="simple", alpha=0.5, h=2)  # alpha = 0.5
plot(window(AirPassengers, start=1956), xlim=c(1956, 1962), 
     main="AirPassengers", type="o")
lines(out$mean, col="red", type="o")
lines(out1$mean, col="blue", type="o")


out <- ses(AirPassengers, initial="simple", h=2)  # alpha estimated by `ses`
summary(out)


# Answer
out <- ses(JohnsonJohnson, initial="simple", h=1)
out$mean























