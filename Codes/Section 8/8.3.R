# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co


library(ggplot2)
data(mpg, package="ggplot2")
# mpg <- read.csv("http://goo.gl/uEeRGu")
# full link: https://raw.githubusercontent.com/selva86/datasets/master/mpg_ggplot2.csv

g <- ggplot(mpg, aes(x=displ, y=hwy)) + 
  geom_point() + 
  geom_smooth(method="lm") + 
  theme_bw()
plot(g)

# Multiple Plots with Facet Wrap and Facet Grid --------------------------------------------
g + facet_wrap( ~ class)
g + facet_wrap( ~ class, scales = "free")
g + facet_grid( ~ class)  

g1 <- g + facet_grid(manufacturer ~ class)
plot(g1)

g2 <- g + facet_grid(cyl ~ class)
plot(g2)


# Combine multiple ggplot objects in sinlge plot ------------------------------------------
library(gridExtra)
gridExtra::grid.arrange(g1, g2, ncol=2)


# Answer -----------------------------------------------------------------------------------
mpg_s <- mpg[mpg$class %in% c("suv", "midsize"), ]

library(ggplot2)
data(mpg, package="ggplot2")
# mpg <- read.csv("http://goo.gl/uEeRGu")
g <- ggplot(mpg_s, aes(x=displ, y=hwy)) + 
  geom_point() + 
  geom_smooth(method="lm") + 
  theme_bw() + 
  facet_grid(class ~ trans)
plot(g)


