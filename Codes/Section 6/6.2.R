# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co


# Augmented Dickey Fuller Test (ADF Test)
library(tseries)
set.seed(100)
x <- runif(1000)  # random noise
plot(x)  # plot the random noise
adf.test(x)  # check if stationary.

adf.test(JohnsonJohnson)
plot(JohnsonJohnson)

# Concept of differencing
library(forecast)
x <- AirPassengers

# Seasonal differencing
ns <- nsdiffs(x)
ns
if(ns > 0) {
  de_seas<- diff(x,lag=frequency(x),differences=ns)
} else {
  de_seas <- x
}

plot(de_seas)

# regular differencing
n <- ndiffs(de_seas)
if(n > 0) {
  differenced <- diff(de_seas, differences=n)
}
plot(differenced)


# stl decomposition
library(forecast)
ts.stl<- stl(AirPassengers,"periodic")  # decompose the TS

# de-trend and de-seasonalize - method 1
de_seas<- AirPassengers - ts.stl$time.series[, 1]
plot(de_seas, type='l', main="De-Seasonalize")

de_trend<- AirPassengers - ts.stl$time.series[, 2]
plot(de_trend, type='l', main="De-Trend")

# de-seasonalize - method 2
ts.sa <- forecast::seasadj(ts.stl)  
plot(ts.sa, type="l")  # seasonal adjusted

# de-trend - method 2
trModel<- lm(AirPassengers ~ c(1:length(AirPassengers)))
de_trend2 <- resid(trModel)
plot(de_trend2, type="l")  # resid(trModel) contains the de-trended series.



# Answer
library(forecast)
ts.stl<- stl(JohnsonJohnson,"periodic")  # decompose the TS
de_trend<- JohnsonJohnson - ts.stl$time.series[, 2]
plot(de_trend, type='l', main="JohnsonJohnson De-Trended")


