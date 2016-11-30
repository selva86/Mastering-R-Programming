# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co

# Code to Reproduce chart
data(Prestige, package = "car")
plot(x=Prestige$education, y=Prestige$income, 
     main = "Income vs Education", 
     ylim = c(0, 10000))
abline(lm(income ~ education, data = Prestige))

# Ccor function
cor(Prestige$income, Prestige$education)

cor.test(Prestige$income, Prestige$education)

boxplot(income ~ type, data=Prestige, 
        main="Income vs Occupation Type", 
        ylab="Income")

anovamod<- aov(income ~ type, data=Prestige)
summary(anovamod)

Prestige$income_cat<- dplyr::ntile(Prestige$income, 4)
table(Prestige$income_cat, Prestige$type)

chisq.test(y=Prestige$income_cat, x=Prestige$type)



