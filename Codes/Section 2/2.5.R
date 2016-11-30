# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co


data(Prestige, package="car")
Prestige <- na.omit(Prestige)

# k-fold cross validation
library(boot)
glmmod <- glm(prestige ~ income + education + type, data=Prestige)
cv.glm(Prestige, glmmod, K=5)$delta

# Challenge 1
glmmod1 <- glm(prestige ~ income + education, data=Prestige)
glmmod2 <- glm(prestige ~ education + type + income, data = Prestige)
glmmod3 <- glm(formula = prestige ~ education + type + income + type:income, data = Prestige)

# Answer.
set.seed(100)
boot::cv.glm(Prestige, glmmod1, K=5)$delta
boot::cv.glm(Prestige, glmmod2, K=5)$delta
boot::cv.glm(Prestige, glmmod3, K=5)$delta
# boot::cv.glm(Prestige, glmmod4, K=5)$delta
