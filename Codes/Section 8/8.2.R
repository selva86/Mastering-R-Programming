# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co


library(ggplot2)
data("midwest", package = "ggplot2")
# midwest <- read.csv("http://goo.gl/G1K41K") 
# Full link: https://raw.githubusercontent.com/selva86/datasets/master/midwest.csv

# Add plot components
gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(aes(col=state, size=popdensity)) + 
  geom_smooth(method="lm", col="firebrick") + 
  coord_cartesian(xlim=c(0, 0.1), ylim=c(0, 1000000)) + 
  labs(title="Area Vs Population", y="Population", x="Area")

# Legend ---------------------------------------------------------------------------------------
# Change Legend Title
gg <- gg + scale_color_discrete(name="State") + scale_size_continuous(name = "Density")

# Change Legend Position
gg + theme(legend.position="left")
gg + theme(legend.position="bottom")
gg + theme(legend.justification=c(1,0), legend.position=c(1,0))  # inside the plot
gg + theme(legend.justification=c(1,1), legend.position=c(1,1))  # inside the plot

# Change styling of legend
gg <- gg + theme(legend.title = element_text(size=12, color = "firebrick"), 
                 legend.text = element_text(size=10),
                 legend.key=element_rect(fill='gray')) + 
  guides(colour = guide_legend(override.aes = list(size=2,  stroke=1.5))) 


# Text -----------------------------------------------------------------------------------------
# make dataset with filtered rows.
midwest_sub <- midwest[midwest$poptotal > 400000, ]
midwest_sub$large_county <- ifelse(midwest_sub$poptotal > 400000, midwest_sub$county, "")

# Plot texts
gg + geom_text(aes(label=large_county), size=2, data=midwest_sub)   # text
gg <- gg + geom_label(aes(label=large_county), size=2, data=midwest_sub, alpha=0.25)   # label
plot(gg)

gg + coord_flip()  # flip the X and Y axis
gg + scale_x_reverse() + scale_y_reverse()  # reverse the scales of X and Y axis.

# Custom Annotation -----------------------------------------------------------------------------
library(grid)
my_text <- "This text is at x=0.7 and y=0.9!"
my_grob = grid.text(my_text, x=0.7,  y=0.9, gp=gpar(col="firebrick", 
                                                    fontsize=10, fontface="bold"))
class(my_grob)
gg + annotation_custom(my_grob)


# Answer ----------------------------------------------------------------------------------------
gg <- ggplot(data=cars, aes(x=speed, y=dist, size=dist)) + 
  geom_point() + geom_smooth() + labs(title="Cars", x="Speed", y="Dist")
print(gg)
my_text <- "The faster the car, the longer it takes to stop\n So maintain a longer safe distance at high speeds."
my_grob = grid.text(my_text, x=0.3,  y=0.85, gp=gpar(col="steelblue", fontsize=10, fontface="bold"))
gg + annotation_custom(my_grob) + theme(legend.position="None")
