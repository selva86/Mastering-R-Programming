# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co


# install.packages(‘mass’)
data(Boston, package="MASS")

# principal components model
pca_out<- prcomp (Boston, scale. = T) # PCA
pca_out

# extract the principal components
boston_pc<- pca_out$x
boston_pc

# summary
summary(pca_out)

# proportion of variance explained by PCs
plot(pca_out)

# Biplot (Relationship between features and obs)
par(mar=c(4,4,2,2))
biplot(pca_out, cex=0.5, cex.axis=0.5)





# Answer:
m_pca<- prcomp (mtcars, scale. = T) # PCA
out <- m_pca$x
summary(m_pca)
