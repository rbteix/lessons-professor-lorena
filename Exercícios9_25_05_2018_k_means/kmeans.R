# k-means algorithm
##################################################################################################

# Adapted from https://bitbucket.org/rodrigo_mello/ml4u/src/ab2300b432ac9105df1352d5890543cf136bef8f/kmeans/kmeans.r?at=master&fileviewer=file-view-default
my.kmeans <- function(dataset, k=2, threshold=1e-3) {
  
  if (is.vector(dataset)) {
    dataset = as.data.frame(matrix(dataset, ncol=1))
  }

  # sampling initial centroids  
  ini = sample(1:nrow(dataset), size=k)
  centroid = as.data.frame(dataset[ini,])
  div = 2 * threshold
  
  while (div > threshold) {
    
    dists = NULL
    for (i in 1:k) { # For each centroid
      # computing euclidean distance from each point to the centroids
      euclidean = apply(dataset, 1, 
                        function(row) { # euclidean distance
                          sqrt(sum((row - centroid[i,])^2))})
      dists = cbind(dists, euclidean)
    }
    
    # Finding to which centroid each point is associated
    associated.centroids = apply(dists, 1, function(row) { which.min(row) } )
    
    # updating centroids
    div = 0
    for (i in 1:k) { # for each centroid
      ids = which(associated.centroids == i) # index of examples in centroid i
      new.position = colMeans(as.data.frame(dataset[ids,]))
      div = div + sqrt(sum((centroid[i,] - new.position)^2)) # computing within distances error
      centroid[i,] = new.position
    }
    div = div / k
    
    cat("Divergence: ", div, "\n")
  }
  
  
  ret = list()
  ret$k = k
  ret$initial = as.data.frame(dataset[ini,])
  ret$centroid = centroid
  ret$clusters = associated.centroids
  
  return (ret)
}

# normalizing iris
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

iris_norm <- as.data.frame(lapply(iris[,1:4], normalize))

# using k-means on iris
result <- my.kmeans(iris_norm[,1:4],k=3)

# plotting results (using just two features for easing visualization)
plot(iris_norm[,3:4],col=result$clusters)
points(result$initial[,3:4],col="orange", pch=7, cex=2)
points(result$centroid[,3:4],col="blue", pch=8, cex=2)

# Exercice: run multiple times to see the differences

# comparing to plot of original labels
plot(iris_norm[,3:4],col=iris$Species)


##########################################################################################################
# Using kmeans from stats package
# Based on: https://www.r-bloggers.com/k-means-clustering-in-r/

res <- kmeans(iris_norm,centers =3,nstart = 20) # nstart tries different initializations and finds the one with lowest within variance
res

plot(iris_norm[,3:4],col=res$cluster)
points(res$centers[,3:4],col="blue", pch=8, cex=2)

table(irisCluster$cluster, iris$Species)

######################################################################################
# Elbow method
# From https://www.r-bloggers.com/finding-optimal-number-of-clusters/

# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
data <- iris_norm
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=20,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#################################################################################
library(mlbench)

# two normals
data <- mlbench.2dnormals(1000,sd=0.5)
plot(data)

data_norm <- as.data.frame(lapply(as.data.frame(data$x[,1:2]), normalize))

res <- kmeans(data_norm,centers =2,nstart = 20) # nstart tries different initializations and finds the one with lowest within variance
res

plot(data_norm[,1:2],col=res$cluster)
points(res$centers[,1:2],col="blue", pch=8, cex=2)

table(data$classes,res$cluster)

# Exercice: change sd in data generation

# smiley
data <- mlbench.smiley()
plot(data)

data_norm <- as.data.frame(lapply(as.data.frame(data$x[,1:2]), normalize))

res <- kmeans(data_norm,centers =4,nstart = 20) # nstart tries different initializations and finds the one with lowest within variance
res

plot(data_norm[,1:2],col=res$cluster)
points(res$centers[,1:2],col="blue", pch=8, cex=2)

table(data$classes,res$cluster)

# spirals
data <- mlbench.spirals(1000)
plot(data)

data_norm <- as.data.frame(lapply(as.data.frame(data$x[,1:2]), normalize))

res <- kmeans(data_norm,centers =2,nstart = 20) # nstart tries different initializations and finds the one with lowest within variance
res

plot(data_norm[,1:2],col=res$cluster)
points(res$centers[,1:2],col="blue", pch=8, cex=2)

table(data$classes,res$cluster)
