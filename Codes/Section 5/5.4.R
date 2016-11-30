# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co


dta <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/00236/seeds_dataset.txt", 
                  header=F, fill=T)

# dta <- read.csv("https://raw.githubusercontent.com/selva86/datasets/master/seeds.csv")  # backup

colnames(dta) <- c("area",
                   "perimeter", 
                   "compactness", 
                   "length_of_kernel", 
                   "width_of_kernel", 
                   "asymmetry_coefficient", 
                   "length_of_kernel_groove",
                   "type") 

# seed_types <- "Kama, Rosa and Canadian"

# Hierarchical clustering
library(cluster)
dist_mat <- dist(dta[, -8])
h <- hclust(dist_mat)
h

# Plot
plot(h, cex=0.75, labels = dta$type)
rect.hclust(h, k = 3, border = 2:4)

pred <- cutree(h, k=3)  # create k clusters
pred



# Answer
distmat <- dist(iris[, -5])
clus <- hclust(distmat, method = "ward.D")
plot(clus)
rect.hclust(clus, k = 3, border = 2:4)

pred <- cutree(clus, k=3)  # create k clusters
pred
table(pred,iris$Species)

























