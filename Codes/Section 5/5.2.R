# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co


# K-Means on Original Data and Principal Components.

library(cluster)
set.seed(100)  # for repeatability.
k_obj <- kmeans(iris[, -5], 3)  # run k-means with k=3
k_obj$cluster

# plot on principal components as X and Y axis
pcmod<- prcomp(iris[, -5], scale=T)
pcas <- predict(pcmod)
plot(pcas[, 1], pcas[, 2], col=k_obj$cluster)
text(pcas[, 1], pcas[, 2], iris$Species, cex=0.5)

# k=2
k_obj<- kmeans(iris[, -5], 2)

# plot on principal components as X and Y axis
pcmod<- prcomp(iris[, -5], scale=T)
pcas<- predict(pcmod)
plot(pcas[, 1], pcas[, 2], col=k_obj$cluster)
text(pcas[, 1], pcas[, 2], iris$Species, cex=0.5)


# Answer
# Compute the PCAs
pcmod<- prcomp(iris[, -5], scale=T)
pcas<- predict(pcmod)

# Clustering
k_pcaobj <- kmeans(pcas, 3)
plot(pcas[, 1], pcas[, 2], col=k_pcaobj$cluster)
text(pcas[, 1], pcas[, 2], iris$Species, cex=0.5)
table(k_pcaobj$cluster, iris$Species)























