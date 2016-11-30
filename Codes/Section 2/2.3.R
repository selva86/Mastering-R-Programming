# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co


# Dataprep URL: https://goo.gl/ZTd9wf
# Alternate Dataprep URL: http://ow.ly/jXnR305qq2v

# Prep
data(Prestige, package="car")
set.seed(100)
train_rows <- sample(1:nrow(Prestige), size=0.7*nrow(Prestige))
training <- Prestige[train_rows, ]
test <- Prestige[-train_rows, ]

# Model
lmmod <- lm(prestige ~ income + education, data=training)
summary(lmmod)
par(mfrow=c(2,2))
plot(lmmod)

lmtest::bptest(lmmod)


cooks.distance(lmmod)
car::influenceIndexPlot(lmmod, id.n = 5)

car::residualPlots(lmmod)

lmmod2 <- lm(prestige ~ log(income) + education, data=training)
car::residualPlots(lmmod2)
summary(lmmod2)



