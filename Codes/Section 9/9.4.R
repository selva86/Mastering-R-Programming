# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co


library(data.table)

# Export dataset with 10M rows
mat <- matrix(sample(10, 100000000, replace = T), ncol=10)
dim(mat)
write.csv(mat, "mat.csv", row.names = F)

# Import (read.csv vs fread) ---------------------------------------------------------
system.time(mat_base <- read.csv("mat.csv"))
system.time(mat_fread <- data.table::fread("mat.csv"))

# Data.Frame to Data.Table -----------------------------------------------------------
data("LifeCycleSavings", package = "datasets")  # load/re-load the data
head(LifeCycleSavings) 
class(LifeCycleSavings)

library(data.table)
data("LifeCycleSavings", package = "datasets")  # load/re-load the data
setDT(LifeCycleSavings)
class(LifeCycleSavings)

data("LifeCycleSavings", package = "datasets")  # load/re-load the data
class(LifeCycleSavings)
LifeCycleSavings$rownames <- rownames(LifeCycleSavings)
lcs <- data.table(LifeCycleSavings)
lcs

# Data Manipulations -------------------------------------------------------------------
# Change column order
setcolorder(lcs, c(6, 1:5))  # change order of columns.
head(lcs)

# Filtering
lcs[lcs$sr > 12 & lcs$sr < 20, ]
lcs[sr > 12 & sr < 20, ]

# Select columns
lcs[sr > 12 & sr < 20, list(rownames, sr, pop15)]
lcs[sr > 12 & sr < 20, .(rownames, sr, pop15)]

# Create a new column
lcs[, pop15_fac:=cut(pop15, breaks=pretty(pop15))]
lcs[, pop75_fac:=cut(pop75, breaks=pretty(pop75))]
head(lcs)

# Grouping
lcs[, .(sr=mean(sr)), by=pop15_fac]

# Filter and then group.
lcs[pop75 > 1, .(sr=mean(sr)), by=pop15_fac]

# sort using order()
lcs[pop75 > 1, .(sr=mean(sr)), by=pop15_fac][order(pop15_fac)]

# sort using keyby
out <- lcs[pop75 > 1, .(sr=mean(sr)), keyby=pop15_fac]
out
key(out)

# Create multiple columns in one go!
lcs[, `:=`(scale_pop15=scale(pop15),
           scale_pop75=scale(pop75))]

head(lcs)

# Pipes within data.table -------------------------------------------------------------------
library(dplyr)
lcs[, `:=`(scale_pop15=scale(pop15) %>% round(2),
           scale_pop75=scale(pop75) %>% round(2))]

head(lcs)

# Column names in a character vector
mycol <- c("c1", "c2")
lcs[, (mycol) := .(sr/mean(sr), pop15/mean(pop15))]
head(lcs)

# Delete columns
lcs[, c2 := NULL]

# Key ---------------------------------------------------------------------------------------
setkey(lcs, pop15_fac)
lcs['(40,45]']

# More than 1 key
setkey(lcs, rownames, pop15_fac)
lcs[.('Bolivia', '(40,45]')]
lcs[.(c('Bolivia', 'Libya'), '(40,45]')]
key(lcs)

# dcast.data.table
dcast.data.table(lcs, pop15_fac ~ pop75_fac, 
                 value.var = "sr", 
                 fun.aggregate = mean)

dcast.data.table(lcs, pop15_fac ~ pop75_fac, 
                 value.var = c("sr", "dpi"), 
                 fun.aggregate = mean)



# Answer ---------------------------------------------------------------------------------
data("USArrests")
USArrests$states <- rownames(USArrests)
arrests <- data.table(USArrests)
arrests[, UrbanPop_fac:=cut(UrbanPop, 4)]
arrests[Assault > 100, .(mean_murder=mean(Murder)), by=UrbanPop_fac]

