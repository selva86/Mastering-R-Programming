# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co

browseVignettes("ggplot2")
library(ggplot2)
library(ggrepel)
data("midwest", package = "ggplot2")
# midwest <- read.csv("http://goo.gl/G1K41K") 

# Add plot components
gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(aes(col=state)) + 
  coord_cartesian(xlim=c(0, 0.1), ylim=c(0, 1000000)) + 
  labs(title="Area Vs Population", y="Population", x="Area") +
  theme(legend.position="None")

midwest_sub <- midwest[midwest$poptotal > 400000, ]
midwest_sub$large_county <- ifelse(midwest_sub$poptotal > 400000, midwest_sub$county, "")


gg + geom_text(aes(label=large_county), size=2, data=midwest_sub)   # text

gg + geom_text_repel(aes(label=large_county), size=2, data=midwest_sub)   # text
gg + geom_label_repel(aes(label=large_county), size=2, data=midwest_sub)   # label


# install.packages("ggfortify")
library(ggfortify)
autoplot(AirPassengers)

mod <- lm(dist ~ speed, data=cars)
autoplot(mod)

kmod <- kmeans(iris[, -5], 3)
autoplot(kmod, data=iris)















