# hierarchical clustering
##################################################################################################

# From: https://www.r-bloggers.com/hierarchical-clustering-in-r-2/
# Aglomerative

# normalizing iris
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

iris_norm <- as.data.frame(lapply(iris[,1:4], normalize))

# using hierarchical clustering on iris (default complete link)
clusters <- hclust(dist(iris[, 1:4]))
plot(clusters,hang=-1)

# cutting three clusters
clusterCut <- cutree(clusters, 3)
rect.hclust(clusters, k=3, border="red")
# comparing to the known labels
table(iris$Species,clusterCut )

# using hierarchical single-linkage clustering on iris 
clusters <- hclust(dist(iris[, 1:4]),method="single")
plot(clusters,hang=-1)

# cutting three clusters
clusterCut <- cutree(clusters, 3)
rect.hclust(clusters, k=3, border="red")
# comparing to the known labels
table(iris$Species,clusterCut )

# using hierarchical average-linkage clustering on iris 
clusters <- hclust(dist(iris[, 1:4]),method="average")
plot(clusters,hang=-1)

# cutting three clusters
clusterCut <- cutree(clusters, 3)
rect.hclust(clusters, k=3, border="red")
# comparing to the known labels
table(iris$Species,clusterCut )

# Exercice: change cluster clut and see what happens

##########################################################################################################
# Divisive
# From: https://www.r-bloggers.com/how-to-perform-hierarchical-clustering-using-r/

library(cluster)
hc4 <- diana(iris_norm)
pltree(hc4, cex = 0.6, hang = -1, main = "Dendrogram of diana")
rect.hclust(clusters, k=3, border="red")
clusterCut <- cutree(hc4, 3)
table(iris$Species,clusterCut )

#################################################################################
library(mlbench)

# two normals
data <- mlbench.2dnormals(1000,sd=0.5)
plot(data)

data_norm <- as.data.frame(lapply(as.data.frame(data$x[,1:2]), normalize))

res <- hclust(dist(data_norm),method="single") # nstart tries different initializations and finds the one with lowest within variance
plot(res,hang=-1)

# cutting three clusters
clusterCut <- cutree(res, 2)
rect.hclust(res, k=2, border="red")

plot(data_norm[,1:2],col=clusterCut)
# comparing to the known labels
table(data$classes,clusterCut)

# Exercice: change hierarchical clustering linkage method

# smiley
data <- mlbench.smiley()
plot(data)

data_norm <- as.data.frame(lapply(as.data.frame(data$x[,1:2]), normalize))

res <- hclust(dist(data_norm),method="single") # nstart tries different initializations and finds the one with lowest within variance
plot(res,hang=-1)

# cutting four clusters
clusterCut <- cutree(res, 4)
rect.hclust(res, k=4, border="red")

plot(data_norm[,1:2],col=clusterCut)
# comparing to the known labels
table(data$classes,clusterCut)

# Exercice: change hierarchical clustering linkage method

# spirals
data <- mlbench.spirals(1000)
plot(data)

data_norm <- as.data.frame(lapply(as.data.frame(data$x[,1:2]), normalize))

res <- hclust(dist(data_norm),method="single") # nstart tries different initializations and finds the one with lowest within variance
plot(res,hang=-1)

# cutting four clusters
clusterCut <- cutree(res, 2)
rect.hclust(res, k=2, border="red")

plot(data_norm[,1:2],col=clusterCut)
# comparing to the known labels
table(data$classes,clusterCut)

# Exercice: change hierarchical clustering linkage method
