# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co


library(apcluster)
dta <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/00236/seeds_dataset.txt", 
                  header=F, fill=T)

# Alternate url: "https://raw.githubusercontent.com/selva86/datasets/master/seeds.csv"

colnames(dta) <- c("area",
                   "perimeter", 
                   "compactness", 
                   "length_of_kernel", 
                   "width_of_kernel", 
                   "asymmetry_coefficient", 
                   "length_of_kernel_groove",
                   "type") 
head(dta)

neg_sim_mat<- negDistMat(dta[, -8], r=2)
dim(neg_sim_mat)

# other options
expSimMat(dta[, -8], r=2, w=1, method="euclidean")  # r is the power, w is the radius
linSimMat(dta[, -8], w=1, method="euclidean")
corSimMat(dta[, -8], r=1, signed=TRUE)
linKernel(dta[, -8], normalize=FALSE)

# Run apcluster
clus <- apcluster(neg_sim_mat)
clus
cl <- clus@clusters  # get the clusters
xmplrs <- clus@exemplars  # exemplars

# Function to tidy the clusters
tidy_clus <- function(cl){
  names(cl) <- paste0("cl", 1:length(cl))  # assign names
  
  # function to get the observation and cluster number in a dataframe.
  getCl <- function(x){
    data.frame(id=cl[[x]], cluster=rep(x, length(cl[[x]])))
  }  
  
  # get the observation and cluster number
  groups_list <- lapply(names(cl), getCl) 
  groups_df <- Reduce(rbind, groups_list)  # row bind the list of dataframes
  groups <- groups_df[order(groups_df$id), ]  # sort by obs. id.
  groups
}

groups <- tidy_clus(cl)

dta_x <- cbind(dta, groups)  # attach it to original dataset
head(dta_x)

# Compare
t1 <- table(dta_x$type, dta_x$cluster)
t1

# Merge some clusters
clus_agg <- aggExCluster(s=neg_sim_mat, x=clus)
plot(clus_agg)

clus_agg_4 <- cutree(clus_agg, k=3)
clus_agg_4

groups2 <- tidy_clus(cl=clus_agg_4@clusters)
dta_xx <- cbind(dta, groups2)
t2 <- table(dta_xx$type, dta_xx$cluster)
t2




# Answer
library(apcluster)
data(iris)

# get the principal components
pca_iris_mod <- princomp(iris[, -5])  # princpal components model
screeplot(pca_iris_mod, type="lines")  # scree plot
pca_iris <- predict(pca_iris_mod)  # compute the prncipal components
head(pca_iris)

# run AP Cluster for predefined K clusters.
iris_clus <- apclusterK(negDistMat(r=2), pca_iris[, 1:2], K=3, prc=0)
cl <- iris_clus@clusters
xmplrs <- iris_clus@exemplars

grps <- tidy_clus(cl)
iris_x <- cbind(iris, grps)
iris_x$Species_abbr <- abbreviate(iris_x$Species, 1)
head(iris_x)

# plot
plot(pca_iris[, 1:2], col=iris_x$cluster, pch="*", 
     main="Iris - AP Cluster")  # draw points
points(pca_iris[xmplrs, 1], pca_iris[xmplrs, 2], 
       col=iris_x[xmplrs, "cluster"], cex=1.5)  # mark exemplars
text(pca_iris[, 1], pca_iris[, 2]-0.1, 
     labels=iris_x$Species_abbr, cex=0.5,  
     col=as.numeric(as.factor(iris_x$Species)))  # mark original group.
