# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co


library(clustertend)  # for hopkins statistic
hopkins(iris[, -5], n=(nrow(iris)-1))  

library(seriation)  # for dissplot
set.seed(100)

# create random matrix and draw dissplot
rand_mat<- matrix(runif(1000), nrow=100)
dissplot(dist(rand_mat), main="Clustering Tendency: dissplot")

# iris
df_dist<- dist(iris[, -5])
dissplot(df_dist, main="Clustering Tendency: dissplot")  # Clear segment seen.


library(cluster)
data(ruspini, package = "cluster")
x <- ruspini
avg_sil_wid<- numeric(NROW(x)/2)  # initialize output of silhouette widths

for(nclus in 2:(nrow(x)/2)){
  set.seed(100)  # for repeatability
  kout<- kmeans(x, nclus)  # run kmeans
  ss<- silhouette(kout$cluster, dist(x))  # Silhouette plot
  avg_sil_wid[nclus]<- mean(ss[, 3])  
}

opt_clusters<- which.max(avg_sil_wid)  # optimal cluster size
opt_clusters

# Plot it
par(mar=c(5,2,3,1))
plot(1:(NROW(x)/2), avg_sil_wid, type = "b", pch = 19, frame = T, 
     xlab = "Number of clusters k", 
     ylab="AvgSilWid", main="Optimal cluster size")
points(x=opt_clusters, y=max(avg_sil_wid), col="red", pch=21, cex=3)
abline(v = which.max(avg_sil_wid), lty = 2)

set.seed(100)  # for repeatability
kout<- kmeans(x, 4)  # run kmeans
kout$cluster
plot(ruspini, col=kout$cluster)



# Answer
# Challenge 1
data("ruspini", package = "cluster")
clustertend::hopkins(ruspini, n=nrow(ruspini)-1)  # [1] 0.2200452
seriation::dissplot(dist(ruspini))

# Challenge 2
x <- faithful
avg_sil_wid<- numeric(NROW(x)/4)
for(nclus in 2:(nrow(x)/4)){
  set.seed(100)  # for repeatability
  kout<- kmeans(x, nclus)  # run kmeans
  ss<- silhouette(kout$cluster, dist(x))  # Silhouette plot
  avg_sil_wid[nclus]<- mean(ss[, 3])  
}
opt_clusters<- which.max(avg_sil_wid)  # optimal cluster size
opt_clusters

# Plot it
par(mar=c(5,2,3,1))
plot(1:(NROW(x)/4), avg_sil_wid, type = "b", pch = 19, frame = T, 
     xlab = "Number of clusters k", ylab="AvgSilWid", main="Optimal cluster size")
points(x=opt_clusters, y=max(avg_sil_wid), col="red", pch=21, cex=3)
abline(v = which.max(avg_sil_wid), lty = 2)