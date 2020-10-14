# Validation measures clustering
#################################################################################

# Adapted from: http://www.sthda.com/english/articles/29-cluster-validation-essentials/98-choosing-the-best-clustering-algorithms/

install.packages("clValid")
library("clValid")
library(mlbench)

# two normals
data <- mlbench.2dnormals(1000,sd=0.5)
plot(data)

df <- scale(data$x[,1:2])

# comparing hierarchical and kmeans with internal validation indexes
summary(intern)

# hierarchical
res <- hclust(dist(df),method="single") # nstart tries different initializations and finds the one with lowest within variance
# cutting four clusters
clusterCut <- cutree(res, 2)
plot(df[,1:2],col=clusterCut)
# comparing to the known labels
table(data$classes,clusterCut)

# kmeans
res <- kmeans(df,centers =2,nstart = 20) # nstart tries different initializations and finds the one with lowest within variance
plot(df[,1:2],col=res$cluster)
table(data$classes,res$cluster)

# smiley
data <- mlbench.smiley()
plot(data)

df <- scale(data$x[,1:2])

# comparing hierarchical and kmeans with internal validation indexes
clmethods <- c("hierarchical","kmeans")
intern <- clValid(df, nClust = 2:6, 
                  clMethods = clmethods, validation = "internal",method="single")

summary(intern)

# hierarchical
res <- hclust(dist(df),method="single") 
# cutting four clusters
clusterCut <- cutree(res, 4)
plot(df[,1:2],col=clusterCut)
# comparing to the known labels
table(data$classes,clusterCut)

# kmeans
res <- kmeans(df,centers =4,nstart = 20) # nstart tries different initializations and finds the one with lowest within variance
plot(df[,1:2],col=res$cluster)
table(data$classes,res$cluster)

###################################################################################################

library("fpc")

data <- iris[,1:4]
df <- scale(data)

# k-means
res <- kmeans(df,centers =3,nstart = 20) 
species <- as.numeric(iris$Species)
clust_stats <- cluster.stats(d = dist(df), species, res$cluster)
plot(df[,3:4],col=res$cluster)
table(iris$Species,res$cluster)
# Corrected Rand index
clust_stats$corrected.rand

# hierarchical
res <- hclust(dist(df),method="single") 
# cutting four clusters
clusterCut <- cutree(res, 3)
plot(df[,3:4],col=clusterCut)
# comparing to the known labels
table(iris$Species,clusterCut)
clust_stats <- cluster.stats(d = dist(df), species, clusterCut)
# Corrected Rand index
clust_stats$corrected.rand

#############################################################################################
# other ways to determine number of clusters
# https://uc-r.github.io/hc_clustering

library("factoextra")

# can change the clustering algorithm in FUN (ex kmeans)
fviz_nbclust(df, FUN = hcut, method = "wss")+geom_vline(xintercept = 3, linetype = 2)
fviz_nbclust(df, FUN = hcut, method = "silhouette")
gap_stat <- clusGap(df, FUN = hcut, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)
