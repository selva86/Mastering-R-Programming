# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co


# Auto.Arima
# prepare training and test windows
train <- window(JohnsonJohnson, start=c(1960), end=c(1975))
test <- window(JohnsonJohnson, start=c(1976), end=c(1980))
fit <- auto.arima(train)  # auto.arima model
fit

# forecast
predicted <- forecast(fit, h=20)$mean
accuracy(predicted, test)

# Xreg
# auto arima with external regressors
fit <- auto.arima(train, xreg = fourier(train, 2))
predicted <- forecast(fit, h=20, xreg=fourier(train, 2, 20))$mean
accuracy(predicted, test)  # calc accuracy

# Answer:
fit <- auto.arima(train, xreg = seasonaldummy(train))
predicted <- forecast(fit, h=20, xreg=seasonaldummy(train, 20))$mean
accuracy(predicted, test)
