# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co


set.seed(100)
vec<- round(runif(100, 1,10), 2)

q_ts<- ts(vec, frequency = 4, start = c(1959, 2)) # frequency 4 => Quarterly Data

m_ts<- ts(vec, frequency = 12, start = 1990) # freq 12 => Monthly data. 
# The start and end params are optional.

y_ts<- ts(vec, start=c(2009), frequency=1) # Yearly Data


# Daily
d_ts <- ts(vec, start=c(2009, 10), frequency=365.25) # Yearly Data
d_ts
plot(d_ts)

# xts daily
library(xts)
d_xts <- xts(vec, as.Date("2009-10-01")+0:99)
plot(d_xts)

set.seed(100)
vec1 <- sample(1:100) 
vec2 <- round(vec1 + runif(100, 10, 20))

library(lubridate)
dates <- seq.Date(ymd("2009-10-01"), length.out = 100, by="day")
# dates2 <- seq.Date(as.Date("2009-10-01"), length.out = 100, by="day")
# dates3 <- seq.Date(anytime::anydate("2009-10-01"), length.out = 100, by="day")

# create df
df<- data.frame(vec1, vec2)
rownames(df) <- as.character(dates)
df
df_xts<- as.xts(df)
plot(df_xts$vec1, main="Plot from XTS obj")
lines(df_xts$vec2)

# Filtering dates
df_xts["2009"]  # get all data for 2009
df_xts["2009-10/"]  # get all data from 2009-10 to end
df_xts["/2009-10"]  # get all data till start to 2009-10

# for stocks prices
to.monthly(df_xts)
to.weekly(df_xts)
to.quarterly(df_xts)
to.yearly(df_xts)


# Decompose
decomposedRes<- decompose(m_ts, type="mult") # use type = "additive" for additive components
plot(decomposedRes)

stlRes<- stl(m_ts, s.window = "periodic", robust = T)
stlRes
stlRes$time.series






# Answer 2
jj <- as.xts(JohnsonJohnson)
to.yearly(jj)



























