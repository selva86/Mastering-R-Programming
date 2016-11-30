# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co


library(ggplot2)
data("midwest", package = "ggplot2")
# midwest <- read.csv("http://goo.gl/G1K41K")  
# Full link: https://raw.githubusercontent.com/selva86/datasets/master/midwest.csv

# Basic Scatterplot and Line of best Fit.
g <- ggplot(midwest, aes(x=area, y=poptotal)) + geom_point() + 
  geom_smooth(method="lm")
plot(g)

# Change X and Y axis limits by deleting the points not in range
g + xlim(c(0,0.1)) + ylim(c(0, 1000000))   # deleted

# Change X and Y axis limits by Zooming in
g1 <- g + coord_cartesian(xlim=c(0,0.1), ylim=c(0, 1000000))  # zoom in
plot(g1)

# Add Title, Subtitle, Caption, X and Y Axis labels.
g1 + labs(title="Area Vs Population\nFrom midwest dataset", 
          subtitle="Line of best fit unchanged",
          caption="Source: Ggplot2",
          y="Population", x="Area")

# Full Function Call
gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(aes(col=state)) + 
  geom_smooth(method="lm", col="firebrick") + 
  coord_cartesian(xlim=c(0,0.1), ylim=c(0, 1000000)) + 
  labs(title="Area Vs Population From midwest dataset", 
       y="Population", x="Area", 
       subtitle="With Line of best fit", caption="Source: Ggplot2") +
  theme(legend.position = "None")


# Change Color Palette
gg + scale_colour_brewer(palette = "Set1")

# Full list of Color Palettes
library(RColorBrewer)
brewer.pal.info

# Change Axis Text
gg + scale_x_continuous(breaks=seq(0, 0.1, 0.01), labels = letters[1:11])

# Format Axis Text
gg <- gg + scale_x_continuous(breaks=seq(0, 0.1, 0.01), 
                        labels = sprintf("%1.2f%%", seq(0, 0.1, 0.01))) + 
  scale_y_continuous(breaks=seq(0, 1000000, 200000))

# Change theme elements
gg1 <- gg + theme(plot.title=element_text(size=12, face="bold", color="steelblue", lineheight=1.2),
                  axis.title.x=element_text(size=10), axis.title.y=element_text(size=10), # axis title
                  axis.text.x=element_text(size=10, angle = 30), axis.text.y=element_text(size=10))  # axis text
plot(gg1)


# Change the entire theme with built-in themes
gg1 + theme_bw()
gg1 + theme_light()



# Answer
library(ggplot2)
data("midwest", package = "ggplot2")
# midwest <- read.csv("http://goo.gl/G1K41K") 

gg <- ggplot(midwest, aes(x=popamerindian, y=popasian)) + 
  geom_point(aes(col=state)) +
  geom_smooth(method="lm", col="firebrick") + 
  xlim(c(0, 1000)) + 
  ylim(c(0, 1000)) +
  labs(title="Population of American Indians Vs Asians", 
       y="Pop. Asian", x="Pop. American Indian")

plot(gg)

gg + theme(plot.title=element_text(size=12, face="bold", 
                                   color="steelblue", lineheight=1.2), 
           axis.title.x=element_text(size=10), axis.title.y=element_text(size=10), 
           axis.text.x=element_text(size=10), axis.text.y=element_text(size=10))

