# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co


# load package and data
library(ggplot2)
data(mpg, package="ggplot2")
# mpg <- read.csv("http://goo.gl/uEeRGu")

# Scatterplot
theme_set(theme_bw())  # pre-set the bw theme.
g <- ggplot(mpg, aes(cty, hwy))
g + geom_point()

dim(mpg)

# Jitter Plot
g + geom_jitter(width = .5, size=1)

# Counts Plot
g + geom_count(col="steelblue")

# Bar Chart
g <- ggplot(mpg, aes(manufacturer))
g + geom_bar()
g + geom_bar(width=0.5, fill="steelblue")
g + geom_bar(width=0.5, aes(fill=class))  # multiple colors within the bar.

# Color Palettes
RColorBrewer::display.brewer.all()  # display available palettes
RColorBrewer::brewer.pal.info # available palette names

# Bar chart with color palette
g + geom_bar(width=0.5, aes(fill=class)) + 
  scale_fill_brewer(palette = "Set3")  # Paette: Set3
g + geom_bar(width=0.5, aes(fill=class)) + 
  scale_fill_brewer(palette = "Spectral") + theme_bw()  # Palette: Spectral 

# Full length bars
g + geom_bar(width=0.5, aes(fill=class), position="fill")  # 100% bars

# Geom_bar makes a histograme with Continuous variable.
g <- ggplot(mpg, aes(cty)) + theme_bw()
g + geom_bar()

# Histogram
g + geom_histogram(binwidth = 0.5)  # change bin width
g + geom_histogram(bins = 100)  # change the number of bins

# Aggregate data
cty_mpg <- aggregate(mpg$cty, by=list(mpg$manufacturer), FUN=mean)
colnames(cty_mpg) <- c("manufacturer", "mileage")
cty_mpg

# Draw plot
g <- ggplot(cty_mpg, aes(x=manufacturer, y=mileage, fill=mileage))
g + geom_bar(stat="identity")

g <- ggplot(mpg, aes(manufacturer, cty))
g + geom_boxplot()

g <- ggplot(mpg, aes(manufacturer, cty))
g + geom_boxplot(aes(fill=class), width=0.5)


g + geom_violin(width=3, aes(fill=class))


library(ggfortify)
ts_data <- ggplot2::fortify(AirPassengers)
ggplot(ts_data, aes(Index, Data)) + geom_line()

# Ribbon
ts_data$y2 <- ts_data$Data + runif(nrow(ts_data), 50, 100)
ggplot(ts_data, aes(Index)) + geom_ribbon(aes(ymin=Data, ymax=y2)) 

# Area
ggplot(ts_data, aes(Index)) + geom_ribbon(aes(ymin=0, ymax=y2), fill="firebrick") 

ggplot(ts_data, aes(Index)) + geom_area(aes(y=y2), fill="firebrick", alpha=0.5) + 
  geom_area(aes(y=Data), fill="firebrick")


# Answer
data("economics", package="ggplot2")
# economics <- read.csv("http://goo.gl/1P5WJk")

ggplot(economics, aes(x=date)) + 
  geom_line(aes(y=psavert, col="personal savings")) + 
  geom_line(aes(y=uempmed, col="employed duration")) + 
  scale_color_manual(name="Economics", values=c("personal savings"="firebrick", 
                                                "employed duration"="steelblue")) + 
  labs(y="Employed duration and Personal savings") +
  theme_bw()

