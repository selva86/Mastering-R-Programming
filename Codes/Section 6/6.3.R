# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co


# lag
AirPassengers
ap_lag1 <- dplyr::lag(AirPassengers, n=1)
ap_lag1

# lead
ap_lead1 <- dplyr::lead(AirPassengers, n=1)
ap_lead1

# ACF
library(forecast)
acfRes<- Acf(AirPassengers) # autocorrelation

# Acf of stationary series
set.seed(100)
x <- runif(100)
Acf(x)

# PACF
pacfRes<- Pacf(AirPassengers)  # partial autocorrelation


# CCF
ccfRes<- ccf(mdeaths, fdeaths, ylab = "cross-correlation") # computes cross correlation between 2 timeseries.




# Answer
forecast::Acf(JohnsonJohnson, 25)
