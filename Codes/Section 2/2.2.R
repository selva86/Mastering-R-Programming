# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co

# DataPrep CODE URL: https://goo.gl/AazGLg
# Alternate DataPrep CODE URL: http://ow.ly/wUm1305qpZx

# data prep
data(Prestige, package="car")
head(Prestige)

set.seed(100)
train_rows<- sample(1:nrow(Prestige), size=0.7*nrow(Prestige))
train_rows
training <- Prestige[train_rows, ]
test <- Prestige[-train_rows, ]

lmmod<- lm(prestige ~ income + education, data=training)
summary(lmmod)  # model summary

AIC(lmmod)  # AIC
BIC(lmmod)  # BIC

car:::vif(lmmod)  # VIF

# Model with interaction term
lmmod_1 <- lm(prestige ~ income + education + income:education, data=training)
summary(lmmod_1)

# Model with power of variable
lmmod <- lm(prestige ~ income + I(education^2), data=training)

# Model without intercept
lmmod <- lm(prestige ~ income + I(education^2) - 1, data=training)
summary(lmmod)

# Model with factor variable
lmmod <- lm(prestige ~ income + education + type, data=training)
summary(lmmod)

# Get fitted values
lmmod <- lm(prestige ~ income + education, data=training)
fitted(lmmod)
lmmod$fitted.values
predict(lmmod, newdata=training)
fitted_vals <- predict(lmmod)

# Get residuals
lmmod$model
training$prestige - fitted_vals
lmmod$residuals