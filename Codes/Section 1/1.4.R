# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co

data(Prestige, package="car")
myVar<- Prestige$income

# 3 times IQR
iqr <- IQR(myVar)
myVar[myVar > (3 * iqr)]

# Outside 1.5 times the 75th %ile.
third_quartile <- quantile(myVar, 0.75)
myVar[myVar > (1.5 * third_quartile)]

myVar<- Prestige$income
myVar[myVar>quantile(myVar, 0.95)] <- quantile(myVar, 0.95)
myVar[myVar<quantile(myVar, 0.05)] <- quantile(myVar, 0.05)

# Answer
myVar<- Prestige$income
myVar[myVar>quantile(myVar, 0.99)] <- quantile(myVar, 0.99)
myVar[myVar<quantile(myVar, 0.01)] <- quantile(myVar, 0.01)













