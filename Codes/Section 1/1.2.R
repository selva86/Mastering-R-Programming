# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co

install.packages("car", repo="http://cran.rstudio.com")  # not run.
data(Prestige, package="car")
mean(Prestige$income)
mean(Prestige$income, na.rm=T)

median(Prestige$income, na.rm=T)
sd(Prestige$income, na.rm=T)
var(Prestige$income, na.rm=T)
quantile(Prestige$income, probs=c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99), na.rm=T)

summary(Prestige)

library(Hmisc)
describe(Prestige)

install.packages("pastecs")
pastecs::stat.desc(Prestige)

# Answer
data(Cars93, package="MASS")
sd(Cars93$Price)/mean(Cars93$Price)
quantile(Cars93$Price, probs=0.95)


















